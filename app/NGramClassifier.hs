{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main where

import Prelude hiding (lookup, getContents, readFile, strip, lines, writeFile)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text, strip, lines, stripPrefix, splitOn, pack, unpack)
import Data.Text.IO (getContents, readFile, hGetContents, hPutStr, writeFile, hPutStrLn)
import Codec.Compression.GZip (compress, decompress)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM)
import System.IO (withFile, IOMode(..), stdin, stderr, openFile, stdout, hClose, Handle(..))
import Codec.Compression.PPM (fromSequences, Model, classifySequence, scoreSequence, updateFromSequences, rectifyModel)
import Codec.Compression.PPM.Utils (lineToInstance, accuracy, microFScore, macroFScore)
import Data.Serialize (encodeLazy, decodeLazy)
import Data.Serialize.Text
import Data.List (sortOn, maximumBy, intercalate)
import Text.Printf (printf)
import Control.Monad.Log

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
  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

evaluateModel :: Model Text Char -> Int -> [(Integer, Text, [Char])] -> Double
evaluateModel model n xs = accuracy golds guesses
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
postprocessTrain xs = concat expanded -- map (\(a, b) -> (0, a, b)) xs
  where
    expanded = map postproc xs

readFileOrStdin :: Maybe String -> IO Text
readFileOrStdin (Just f) = case suf of "gz" -> (liftM (pack . BS.unpack . decompress . BS.pack . unpack) . readFile) f
                                       _ -> readFile f
  where
    suf = (reverse . take 2 . reverse) f
readFileOrStdin Nothing = getContents

writeFileOrStdout :: Maybe String -> Text -> IO ()
writeFileOrStdout (Just f) s = case suf of "gz" -> writeFile f ((pack . BS.unpack . compress . BS.pack . unpack) s)
                                           _ -> writeFile f s
  where
    suf = (reverse . take 2 . reverse) f
writeFileOrStdout Nothing s = hPutStr stdout s


main :: IO ()
main = do
  ps <- unwrapRecord "Do PPM-related stuff: all data files should be tab-separated lines of the form  ID<TAB>LABEL<TAB>TEXT  Specify a subcommand with '--help' to see its options."  
  case ps of
    Train {..} -> do
      trainInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) trainFile
      let model = fromSequences n trainInstances
      writeFileOrStdout modelFile ((pack . show) model)
    Update {..} -> do
      case length $ catMaybes [modelFile, trainFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of (--trainFile|--modelFile)!"
                                                        _ -> do
                                                          trainInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) trainFile
                                                          model <- (liftM (read . unpack) . readFileOrStdin) modelFile :: IO (Model Text Char)
                                                          let model' = updateFromSequences model n trainInstances :: Model Text Char
                                                          writeFileOrStdout updatedModelFile ((pack . show) model')
    Apply {..} -> do
      case length $ catMaybes [modelFile, testFile] of 0 -> hPutStrLn stderr "Error: You must specify at least one of (--testFile|--modelFile)!"
                                                       _ -> do
                                                         testInstances <- map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) testFile
                                                         model <- (liftM (read . unpack) . readFileOrStdin) modelFile :: IO (Model Text Char)
                                                         let scores = map (scoreSequence model n . snd) testInstances
                                                             guesses = map (fst . maximumBy (\(_, x) (_, y) -> compare x y) . Map.toList) scores              
                                                         writeFileOrStdout scoresFile (pack (formatScores (zip3 guesses testInstances scores)))
    Evaluate {..} -> do
      case length $ catMaybes [modelFile, testFile] of 2 -> hPutStrLn stderr "Error: You must specify at least one of --testFile|--modelFile!"
                                                       _ -> do
                                                         testInstances <- (liftM postprocessTrain) $ map lineToInstance <$> (liftM (lines . strip) . readFileOrStdin) testFile
                                                         model <- (liftM (read . unpack) . readFileOrStdin) modelFile :: IO (Model Text Char)
                                                         hPutStrLn stderr (pack $ printf "Accuracy: %.3f" (evaluateModel model n testInstances))
