{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Codec.Compression.PPM ( Model
                             , fromSequences
                             , classifySequence
                             , scoreSequence
                             ) where

import Prelude hiding (lookup)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Sequence ((|>))
import qualified Codec.Compression.PPM.Trie as Trie
import Codec.Compression.PPM.Trie (Trie(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Codec.Compression.PPM.Utils (revWindows)
import qualified Data.Maybe as Maybe
import Data.List (sortOn, maximumBy)
import Control.Monad (join)
import Debug.Trace (traceShowId)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Entry a = Entry a | Start deriving (Show, Read, Ord, Eq, Generic)

instance (Serialize a, Ord a) => Serialize (Entry a)

type Model l a = Trie (Entry a) (Map l Integer)


classifySequence :: (Ord l, Ord a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> Int -> [a] -> l
classifySequence m n xs = label
  where
    scores = Map.toList $ scoreSequence m n xs
    label = fst $ maximumBy (\(_, x) (_, y) -> compare x y) scores


scoreSequence :: (Ord l, Ord a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> Int -> [a] -> Map l Double
scoreSequence m n xs = total
  where
    xs' = map Entry xs
    grams = revWindows n xs'
    scores = map (scoreGram m) grams
    total = Map.unionsWith (+) (scores)


oneTerm :: (Ord l, Show l) => Map l Integer -> Map l Integer -> Map l (Maybe Float)
oneTerm numers denoms = Map.empty


scoreGram :: (Ord l, Ord a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> [(Entry a)] -> Map l Double
scoreGram tr ns@(_:ns') = Map.map (toProb 256) vals
  where
    numer = Map.map tail (toCounts tr ns)
    denom = toCounts tr ns'
    n = length ns
    inf = repeat 0
    vals = Map.intersectionWith (\a b -> reverse $ take n $ zip (a ++ inf) (b ++ inf)) numer denom


toProb :: Int -> [(Integer, Integer)] -> Double
toProb alph xs = go xs 0.0
  where
    go [] acc = acc + (log (1.0 / fromIntegral alph))
    go ((0, 0):xs') acc = go xs' (acc + (log (1.0 / 2.0)))
    go ((0, d):xs') acc = go xs' (acc + (log (1.0 / (fromIntegral d + 1.0))))
    go ((n, d):xs') acc = (acc + (log ((fromIntegral n) / (fromIntegral d + 1.0))))


toCounts :: (Ord l, Ord a, Show l, Show a) => Trie a (Map l Integer) -> [a] -> Map l [Integer]
toCounts tr xs = go start tr xs
  where
    start = Map.fromList [(l, []) | l <- (Map.keys . value) tr]
    go acc Trie{..} cs = case cs of
                           [] -> acc'
                           (c:cs') -> case edges Map.!? c of
                                        Nothing -> acc'
                                        Just tr' -> go acc' tr' cs'
      where
        lvalue = Map.map (\x -> [x]) value
        acc' = Map.unionWith (\a b -> a ++ b) acc lvalue


fromSequences :: (Ord l, Ord a, Show l, Show a) => Int -> [(l, [a])] -> Trie (Entry a) (Map l Integer)
fromSequences n xs = model
  where
    xs' = map (\(l, is) -> [(l, x) | x <- revWindows n (replicate (n - 1) Start ++ (map Entry is))]) xs
    model = Trie.labeledSuffixCountTrie (concat xs')
    
