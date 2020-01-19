{-# LANGUAGE OverloadedStrings #-}


module Codec.Compression.PPM.Utils ( lineToInstance
                                   , lineToTokenInstance
                                   , revWindows
                                   , accuracy
                                   , microFScore
                                   , macroFScore
                                   ) where


import Prelude hiding (drop)
import Data.Text (Text, unpack, drop, breakOn)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShowId)

-- | Calculates accuracy
accuracy :: (Eq a) => [a] -> [a] -> Double
accuracy golds guesses = correct / total
  where
    total = (fromIntegral . length) golds
    correct = (fromIntegral . length) $ [x | (x, y) <- zip golds guesses, x == y]


-- | Calculates micro F-Score
microFScore :: (Eq a) => [a] -> [a] -> Double
microFScore guess gold = 1.0
  where
    precs = []
    recs = []


-- | Calculates macro F-Score
macroFScore :: (Eq a, Ord a, Show a) => [a] -> [a] -> Map a Double
macroFScore guess gold = Map.fromList scores
  where
    uvals = (Set.toList . Set.fromList) (guess ++ gold)
    scores = [(v, macroFScore' v guess gold) | v <- uvals]

macroFScore' :: (Show a, Eq a, Ord a) => a -> [a] -> [a] -> Double
macroFScore' v guess gold = if true_pos == 0 then 0 else (2 * (prec * recall) / denom)
  where
    true_pos = (fromIntegral . length) [a == b && a == v | (a, b) <- zip guess gold, a == b && a == v]
    false_pos = (fromIntegral . length) [a /= b && a == v | (a, b) <- zip guess gold, a /= b && a == v]
    false_neg = (fromIntegral . length) [a /= b && b == v | (a, b) <- zip guess gold, a /= b && b == v]
    prec = true_pos / (true_pos + false_pos)
    recall = true_pos / (true_pos + false_neg)
    denom = prec + recall

-- | Splits a line of format ID<TAB>LABEL<TAB>TEXT into a
--   (label, document) tuple of (Text, [Char]).
--   If LABEL is prefixed with a "-", treat as a negative
--   label.
lineToInstance :: Text -> (Text, [Char])
lineToInstance l = (label, unpack (drop 1 text))
  where
    (id, rest) = breakOn "\t" l
    (label, text) = breakOn "\t" (drop 1 rest)


-- | Splits a line of format ID<TAB>LABEL<TAB>TEXT into a
--   (label, document) tuple of (Text, [Char]).
--   If LABEL is prefixed with a "-", treat as a negative
--   label.
lineToTokenInstance :: Text -> (Text, [String])
lineToTokenInstance l = (label, words $ unpack (drop 1 text))
  where
    (id, rest) = breakOn "\t" l
    (label, text) = breakOn "\t" (drop 1 rest)


-- | Returns all subsequences of a given length.
--   Includes initial shorter sequences.
windows :: Int -> [a] -> [[a]]
windows n0 = go 0 Seq.empty
  where
    go n s (a:as) | n' <= n0   = toList s'  : go n' s'  as
                  | otherwise =  toList s'' : go n  s'' as
      where
        n'  = n + 1
        s'  = s |> a
        s'' = Seq.drop 1 s'
    go _ _ [] = []


-- | Reverse-order windows of given length from input sequence.
--   Includes shorter initial windows.
revWindows :: Int -> [i] -> [[i]]
revWindows n is = is'
  where
    is' = (map reverse . windows n) is
