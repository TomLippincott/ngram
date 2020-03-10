{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (lookup, getContents, readFile, strip, lines, writeFile, Word(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text, strip, lines, stripPrefix, splitOn, pack, unpack, breakOn)
import Data.Text.IO (getContents, readFile, hGetContents, hPutStr, writeFile, hPutStrLn)
import Codec.Compression.GZip (compress, decompress)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM)
import System.IO (withFile, IOMode(..), stdin, stderr, openFile, stdout, hClose, Handle(..))
import Codec.Compression.PPM (fromSequences, Model, classifySequence, scoreSequence, updateFromSequences, rectifyModel, Entry(..), Element(..), Word(..), Byte(..), Server(..), Trie(..), lineToInstance, accuracy, microFScore, macroFScore, readBSFileOrStdin, readFileOrStdin, postprocessTrain, formatScores, writeFileOrStdout, writeBSFileOrStdout)
import Data.Serialize (encodeLazy, decodeLazy)
import Data.Serialize.Text
import Data.List (sortOn, maximumBy, intercalate)
import Text.Printf (printf)
import Control.Monad.Log hiding (Handler)
import Debug.Trace (traceShowId)
import Yesod (warp)
import TH.Derive
import Data.Store
import qualified Data.ByteString as BSS
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap
import Data.Proxy (Proxy(..))
import Data.IORef (newIORef)
import Control.DeepSeq (deepseq, force, ($!!), NFData, NFData1)

data Parameters w = Train { trainFile :: w ::: Maybe String <?> "Training data file"
                          , n :: w ::: Int <?> "Maximum context size"
                          , modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                          , modelType :: w ::: Maybe String <?> "Model type, one of 'bytes', 'chars', or 'words' (default: 'chars')"
                          }
                  | Update { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                           , trainFile :: w ::: Maybe String <?> "Training data file"
                           , n :: w ::: Int <?> "Maximum context size"
                           , updatedModelFile :: w ::: Maybe String <?> "File to write updated model to (stdout if unspecified)"
                           , modelType :: w ::: Maybe String <?> "Model type, one of 'bytes', 'chars', or 'words' (default: 'chars')"                           
                           }
                  | Evaluate { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                             , n :: w ::: Int <?> "Maximum context size"
                             , testFile :: w ::: Maybe String <?> "Test data file"
                             , modelType :: w ::: Maybe String <?> "Model type, one of 'bytes', 'chars', or 'words' (default: 'chars')"
                             }
                  | Apply { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                          , testFile :: w ::: Maybe String <?> "Test data file"
                          , n :: w ::: Int <?> "Maximum context size"
                          , scoresFile :: w ::: Maybe String <?> "File to write scores to (stdout if unspecified)"
                          , modelType :: w ::: Maybe String <?> "Model type, one of 'bytes', 'chars', or 'words' (default: 'chars')"                          
                          }
                  | Serve { port :: w ::: Maybe Int <?> "Server port number to bind to"                          
                          , hostName :: w ::: Maybe String <?> "Server host name to bind to"
                          , modelType :: w ::: Maybe String <?> "Model type, one of 'bytes', 'chars', or 'words' (default: 'chars')"                          
                          }
  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

main :: IO ()
main = do
  ps <- unwrapRecord "Do PPM-related stuff: all data files should be tab-separated lines of the form  ID<TAB>LABEL<TAB>TEXT  Specify a subcommand with '--help' to see its options."
  let modelType' = modelType ps
  case modelType ps of Just "words" -> main' (Proxy :: Proxy String) ps "words"
                       Just "bytes" -> main' (Proxy :: Proxy Byte) ps "bytes"
                       Just "chars" -> main' (Proxy :: Proxy Char) ps "chars"
                       Nothing -> main' (Proxy :: Proxy Char) ps "chars"
                       _ -> error "--modelType must be one of (bytes|words|chars)"

main' :: forall e . (NFData e, Element e, Ord e, Hashable e, Read e, Show e, Store (Model Text e)) => Proxy e -> Parameters Unwrapped -> String -> IO ()
main' proxy ps modelType' = do
  case ps of
    Train {..} -> do
      trainInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) trainFile :: IO [(Integer, Text, [e])]
      let model = fromSequences n trainInstances :: Model Text e
      writeBSFileOrStdout modelFile (encode model)
    Update {..} -> do
      case length $ catMaybes [modelFile, trainFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of (--trainFile|--modelFile)!"
                                                        _ -> do
                                                          trainInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) trainFile
                                                          Right model <- readBSFileOrStdin modelFile
                                                          let model' = updateFromSequences model n trainInstances :: Model Text e
                                                          writeBSFileOrStdout updatedModelFile (encode model')                                                          
    Apply {..} -> do
      case length $ catMaybes [modelFile, testFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of (--testFile|--modelFile)!"
                                                       _ -> do
                                                         Right model <- readBSFileOrStdin modelFile :: IO (Either PeekException (Model Text e))
                                                         testInstances <- map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) testFile
                                                         let scores = map (scoreSequence model n . snd) testInstances
                                                             guesses = map (fst . maximumBy (\(_, x) (_, y) -> compare x y) . Map.toList) scores              
                                                         writeFileOrStdout scoresFile (pack (formatScores (zip3 guesses testInstances scores)))
    -- Evaluate {..} -> do
    --   case length $ catMaybes [modelFile, testFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of --testFile|--modelFile!"
    --                                                    _ -> do
    --                                                      testInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) testFile
    --                                                      Right model <- readBSFileOrStdin modelFile :: IO (Either PeekException (Model Text e))
    --                                                      let scores = undefined --evaluateModel model n testInstances
    --                                                          scores' = map snd (Map.toList scores)
    --                                                          macro = (sum scores') / (fromIntegral $ length scores')
    --                                                      hPutStrLn stderr (pack $ printf "Macro f-score: %.3f" macro)
    Serve {..} -> do
      let hostName' = fromMaybe "0.0.0.0" hostName
          port' = fromMaybe 8080 port
      runServer proxy hostName' port' (pack modelType')


runServer :: forall e . (NFData e, Element e, Ord e, Hashable e, Show e, Store (Model Text e), Read e) => Proxy e -> String -> Int -> Text -> IO ()
runServer _ hostName port modelType = do
  modelRef <- newIORef (fromSequences 3 [])
  nRef <- newIORef 3
  modelTypeRef <- newIORef modelType
  warp port (Server modelRef nRef modelTypeRef :: Server e)

