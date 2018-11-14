{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main where

import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import qualified Data.Maybe as M
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Codec.Compression.GZip (compress, decompress)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM)
import qualified Data.Map as Map
import System.IO (withFile, hPutStr, IOMode(..), stdin)
import Codec.Compression.PPM (fromSequences, Model, classifySequence, scoreSequence)
import Codec.Compression.PPM.Utils (lineToInstance, accuracy, microFScore, macroFScore)
import Data.Serialize (encodeLazy, decodeLazy)
import Data.Serialize.Text
import Data.List (sortOn, maximumBy, intercalate)
import Text.Printf (printf)

data Parameters w = Train { trainFile :: w ::: String <?> "Training data file"
                          , devFile :: w ::: String <?> "Development data file"
                          , n :: w ::: Int <?> "Maximum context size"
                          , modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                          }
                  | Evaluate { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                             , n :: w ::: Int <?> "Maximum context size"                          
                             , testFile :: w ::: Maybe String <?> "Test data file"
                             }
                  | Apply { modelFile :: w ::: Maybe String <?> "Model file (output or input, depending on whether training or applying, respectively)"
                          , n :: w ::: Int <?> "Maximum context size"                          
                          , testFile :: w ::: Maybe String <?> "Test data file"
                          }
  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

evaluateModel :: Model T.Text Char -> Int -> [(T.Text, [Char])] -> Double
evaluateModel model n xs = accuracy golds guesses
  where
    golds = map fst xs
    guesses = map (classifySequence model n . snd) xs

formatScores :: [(T.Text, (T.Text, [Char]), Map T.Text Double)] -> String
formatScores xs = intercalate "\n" (map (\(g, (l, t), ps) -> printf "%s\t%s\t%s\t%s" l g t (formatProb ps)) xs)
  where
    formatProb ps = intercalate " " (map (\(k, v) -> printf "%s=%.6f" k v) (Map.toList ps))

main :: IO ()
main = do
  ps <- unwrapRecord "Do PPM-related stuff: all data files should be tab-separated lines of the form  ID<TAB>LABEL<TAB>TEXT  Specify a subcommand with '--help' to see its options."  
  case ps of
    Train {..} -> do
      trainInstances <- map lineToInstance <$> (liftM T.lines . liftM T.strip . T.readFile) trainFile
      let model = fromSequences n trainInstances
      devInstances <- map lineToInstance <$> (liftM T.lines . liftM T.strip . T.readFile) devFile
      print $ evaluateModel model n devInstances
      withFile (M.fromMaybe "model.gz" modelFile) WriteMode (\ h -> BS.hPutStr h ((compress . encodeLazy) model))
    Apply {..} -> do
      --testInstances <- map lineToInstance <$> (liftM T.lines . liftM T.strip . T.readFile) testFile
      let modelFile' = case modelFile of Nothing -> "model.gz"
                                         Just f -> f
      testInstances <- map lineToInstance <$> (liftM T.lines . liftM T.strip) T.getContents
      loader <- (decodeLazy . decompress) <$> BS.readFile modelFile' :: IO (Either String (Model T.Text Char))      
      case loader of
        Right model -> do
          let scores = map (scoreSequence model n . snd) testInstances
              guesses = map (fst . maximumBy (\(_, x) (_, y) -> compare x y) . Map.toList) scores              
          putStrLn (formatScores (zip3 guesses testInstances scores))
        Left error -> print error
    Evaluate {..} -> do
      let modelFile' = case modelFile of Nothing -> "model.gz"
                                         Just f -> f      
      testInstances <- map lineToInstance <$> (liftM T.lines . liftM T.strip) T.getContents
      --testInstances <- map lineToInstance <$> (liftM T.lines . liftM T.strip . T.readFile) testFile
      loader <- (decodeLazy . decompress) <$> BS.readFile modelFile' :: IO (Either String (Model T.Text Char))      
      case loader of
        Right model -> printf "Accuracy: %.3f\n" (evaluateModel model n testInstances)
        Left error -> print error
