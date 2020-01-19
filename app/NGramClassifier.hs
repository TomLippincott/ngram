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

module Main where

import Prelude hiding (lookup, getContents, readFile, strip, lines, writeFile)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text, strip, lines, stripPrefix, splitOn, pack, unpack, breakOn)
import Data.Text.IO (getContents, readFile, hGetContents, hPutStr, writeFile, hPutStrLn)
import Codec.Compression.GZip (compress, decompress)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM)
import System.IO (withFile, IOMode(..), stdin, stderr, openFile, stdout, hClose, Handle(..))
import Codec.Compression.PPM (fromSequences, Model, classifySequence, scoreSequence, updateFromSequences, rectifyModel, Entry(..))
import Codec.Compression.PPM.Trie (Trie(..))
import Codec.Compression.PPM.Utils (lineToInstance, accuracy, microFScore, macroFScore, lineToTokenInstance)
import Data.Serialize (encodeLazy, decodeLazy)
import Data.Serialize.Text
import Data.List (sortOn, maximumBy, intercalate)
import Text.Printf (printf)
import Control.Monad.Log hiding (Handler)
import Debug.Trace (traceShowId)
import Yesod hiding (Update)
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo, rawPathInfo,
                     queryString, requestMethod, responseLBS)
import Data.Aeson.Types (Result(..))
import Data.Aeson.Types
import TH.Derive
import Data.Store
import qualified Data.ByteString as BSS
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap


data Parameters w = Train { trainFile :: w ::: Maybe String <?> "Training data file"
                          , n :: w ::: Int <?> "Maximum context size"
                          , modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                          }
                  | Update { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                           , trainFile :: w ::: Maybe String <?> "Training data file"
                           , n :: w ::: Int <?> "Maximum context size"
                           , updatedModelFile :: w ::: Maybe String <?> "File to write updated model to (stdout if unspecified)"
                           }                    
                  | Evaluate { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                             , n :: w ::: Int <?> "Maximum context size"                          
                             , testFile :: w ::: Maybe String <?> "Test data file"
                             }
                  | Apply { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                          , testFile :: w ::: Maybe String <?> "Test data file"
                          , n :: w ::: Int <?> "Maximum context size"
                          , scoresFile :: w ::: Maybe String <?> "File to write scores to (stdout if unspecified)"
                          }
                  | Serve { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                          , port :: w ::: Maybe Int <?> "Server port number to bind to"
                          , hostName :: w ::: Maybe String <?> "Server host name to bind to"
                          , n :: w ::: Int <?> "Maximum context size"
                          }

  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

$($(derive [d|
   instance Deriving (Store (Entry Char))
   |]))

$($(derive [d|
   instance Deriving (Store (Trie (Entry Char) (Map Text Integer)))
   |]))

data ServeModel = ServeModel (Model Text Char) Int
mkYesod "ServeModel" [parseRoutes|
/ ServeR POST
|]

instance Yesod ServeModel

postServeR = do
  req <- waiRequest
  (res :: Result Value) <- parseCheckJsonBody
  case res of (Success (Object jsobj)) -> do
                (ServeModel model n) <- getYesod
                let (String text) = jsobj HMap.! "text"
                    text' = unpack text
                    scores = Map.toList $ scoreSequence model n text'
                    scores' = [k .= v|(k, v) <- scores]
                    guess = (unpack . snd . maximum) $ [(v, k) | (k, v) <- scores]
                return $ object [("text" :: Text) .= (text' :: String), ("guess" :: Text) .= (guess :: String), ("scores" :: Text) .= (object scores')]
              _ -> do
                return $ object [("error" :: Text) .= ("Malformed request" :: String)]

evaluateModel :: Model Text Char -> Int -> [(Integer, Text, [Char])] -> Map Text Double
evaluateModel model n xs = macroFScore golds guesses
  where
    xs' = [(l, t) | (_, l, t) <- xs]
    golds = map fst xs'
    guesses = map (classifySequence model n . snd) xs'

formatScores :: [(Text, (Text, [Char]), Map Text Double)] -> String
formatScores xs = intercalate "\n" (map (\(g, (l, t), ps) -> printf "%s\t%s\t%s\t%s" l g t (formatProb ps)) xs)
  where
    formatProb ps = intercalate " " (map (\(k, v) -> printf "%s=%.6f" k v) (Map.toList ps))

postproc :: (Text, [Char]) -> [(Integer, Text, [Char])]
postproc (l, cs) = [(v, l', cs) | (v, l') <- labels]
  where
    labels = [case stripPrefix "-" l' of Nothing -> (1, l'); Just l'' -> (-1, l'') | l' <- splitOn "," l]

postprocessTrain :: [(Text, [Char])] -> [(Integer, Text, [Char])]
postprocessTrain xs = concat expanded
  where
    expanded = map postproc xs

readFileOrStdin :: Maybe String -> IO Text
readFileOrStdin (Just f) = case suf of "gz" -> (liftM (pack . BS.unpack . decompress . BS.pack . unpack) . readFile) f
                                       _ -> readFile f
  where
    suf = (reverse . take 2 . reverse) f
readFileOrStdin Nothing = getContents

readFileOrStdinTwo :: Maybe String -> IO (Text, Text)
readFileOrStdinTwo f = do
  txt <- readFileOrStdin f
  return $ breakOn "\n\n" txt

writeFileOrStdout :: Maybe String -> Text -> IO ()
writeFileOrStdout (Just f) s = case suf of "gz" -> writeFile f ((pack . BS.unpack . compress . BS.pack . unpack) s)
                                           _ -> writeFile f s
  where
    suf = (reverse . take 2 . reverse) f
writeFileOrStdout Nothing s = hPutStr stdout s

writeBSFileOrStdout :: Maybe String -> BSS.ByteString -> IO ()
writeBSFileOrStdout (Just f) s = BSS.writeFile f s
writeBSFileOrStdout Nothing s = BSS.hPutStr stdout s

readBSFileOrStdin :: Maybe String -> IO (Either PeekException (Model Text Char))
readBSFileOrStdin (Just f) = decode <$> BSS.readFile f
readBSFileOrStdin Nothing = decode <$> BSS.getContents


main :: IO ()
main = do
  ps <- unwrapRecord "Do PPM-related stuff: all data files should be tab-separated lines of the form  ID<TAB>LABEL<TAB>TEXT  Specify a subcommand with '--help' to see its options."  
  case ps of
    Train {..} -> do
      trainInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) trainFile
      let model = fromSequences n trainInstances
      writeBSFileOrStdout modelFile (encode model)
    Update {..} -> do
      case length $ catMaybes [modelFile, trainFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of (--trainFile|--modelFile)!"
                                                        _ -> do
                                                          trainInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) trainFile
                                                          Right model <- readBSFileOrStdin modelFile
                                                          let model' = updateFromSequences model n trainInstances :: Model Text Char
                                                          writeBSFileOrStdout updatedModelFile (encode model')                                                          
    Apply {..} -> do
      case length $ catMaybes [modelFile, testFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of (--testFile|--modelFile)!"
                                                       _ -> do
                                                         Right model <- readBSFileOrStdin modelFile
                                                         testInstances <- map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) testFile
                                                         let scores = map (scoreSequence model n . snd) testInstances
                                                             guesses = map (fst . maximumBy (\(_, x) (_, y) -> compare x y) . Map.toList) scores              
                                                         writeFileOrStdout scoresFile (pack (formatScores (zip3 guesses testInstances scores)))
    Evaluate {..} -> do
      case length $ catMaybes [modelFile, testFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of --testFile|--modelFile!"
                                                       _ -> do
                                                         testInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) testFile
                                                         Right model <- readBSFileOrStdin modelFile
                                                         let scores = evaluateModel model n testInstances
                                                             scores' = map snd (Map.toList scores)
                                                             macro = (sum scores') / (fromIntegral $ length scores')
                                                         hPutStrLn stderr (pack $ printf "Macro f-score: %.3f" macro)
    Serve {..} -> do
      Right model <- readBSFileOrStdin modelFile
      let port' = fromMaybe 8080 port
      warp port' (ServeModel model n)
