{-# LANGUAGE OverloadedStrings #-}


module Codec.Compression.PPM.Utils ( lineToInstance
                                   , revWindows
                                   , accuracy
                                   , microFScore
                                   , macroFScore
                                   ) where


import qualified Data.Text.Lazy as T
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Foldable (toList)


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
macroFScore :: (Eq a) => [a] -> [a] -> Double
macroFScore guess gold = 1.0
  where
    precs = []
    recs = []


-- | Splits a line of format ID<TAB>LABEL<TAB>TEXT into a
--   (label, document) tuple of (Text, [Char]).
lineToInstance :: T.Text -> (T.Text, [Char])
lineToInstance l = (label, T.unpack (T.drop 1 text))
  where
    (id, rest) = T.breakOn "\t" l
    (label, text) = T.breakOn "\t" (T.drop 1 rest)


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
