{-# LANGUAGE OverloadedStrings #-}


module Codec.Compression.PPM.Utils ( lineToInstance
                                   , revWindows
                                   ) where


import qualified Data.Text.Lazy as T
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Foldable (toList)


--classify :: [


-- | Calculates micro F-Score
microFScore :: [a] -> [a] -> Double
microFScore guess gold = error "unimp"


-- | Calculates macro F-Score
macroFScore :: [a] -> [a] -> Double
macroFScore guess gold = error "unimp"


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
