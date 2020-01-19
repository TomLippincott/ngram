{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Codec.Compression.PPM ( Model(..)
                             , fromSequences
                             , updateFromSequences                             
                             , classifySequence
                             , scoreSequence
                             , rectifyModel
                             , Entry(..)
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
import Codec.Compression.PPM.Utils (revWindows)
import qualified Data.Maybe as Maybe
import Data.List (sortOn, maximumBy)
import Control.Monad (join)
import Debug.Trace (traceShowId)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map as Map


data Entry a = Entry a | Start deriving (Show, Read, Ord, Eq, Generic)
instance Hashable a => Hashable (Entry a)
instance (Serialize a, Ord a) => Serialize (Entry a)
type Model l a = Trie (Entry a) (Map l Integer)


-- |
--
rectifyModel :: (Show l, Ord l, Show a, Ord a) => Model l a -> Model l a
rectifyModel Trie{..} = Trie (Map.filter (\c -> c /= 0) value') (Map.map rectifyModel edges)
  where
    mv = minimum (Map.elems value)
    value' = Map.map (\c -> c - mv) value


-- |
--
classifySequence :: (Ord l, Ord a, Hashable l, Hashable a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> Int -> [a] -> l
classifySequence m n xs = label
  where
    scores = Map.toList $ scoreSequence m n xs
    label = fst $ maximumBy (\(_, x) (_, y) -> compare x y) scores


-- |
--
scoreSequence :: (Ord l, Ord a, Hashable l, Hashable a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> Int -> [a] -> Map l Double
scoreSequence m n xs = total
  where
    xs' = map Entry xs
    grams = revWindows n xs'
    scores = map (scoreGram m) grams
    total = Map.unionsWith (+) scores    


-- | Given a suffix trie of counts and a gram, return the score of that gram for each label
--
scoreGram :: (Ord l, Ord a, Hashable l, Hashable a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> [(Entry a)] -> Map l Double
scoreGram tr ns@(_:ns') = Map.map (toProb 256) vals
  where
    numer = Map.map tail (toCounts tr ns)
    denom = toCounts tr ns'
    n = length ns
    inf = repeat 0
    vals = Map.intersectionWith (\a b -> reverse $ take n $ zip (a ++ inf) (b ++ inf)) numer denom


-- | Calculate PPM probability from a list of item/context count pairs
--
toProb :: Int -> [(Integer, Integer)] -> Double
toProb alpha cts = go cts 0.0
  where
    -- unknown context (escape probability)
    go ((_, 0):cts') acc = go cts' (acc + (log (1.0 / 2.0)))
    -- item unknown in known context (add-one smoothing)
    go ((0, d):cts') acc = go cts' (acc + (log (1.0 / (fromIntegral d + 1.0))))
    -- known context and item (stop recursing)
    go ((n, d):cts') acc = (acc + (log ((fromIntegral n) / (fromIntegral d + 1.0))))
    -- completely unknown item (final escape probability, stop recursing)
    go [] acc = acc + (log (1.0 / fromIntegral alpha))


-- | Extract a gram's count sequence from a suffix trie
--
toCounts :: (Ord l, Ord a, Hashable l, Hashable a, Show l, Show a) => Trie a (Map l Integer) -> [a] -> Map l [Integer]
toCounts tr gram = go start tr gram
  where
    start = Map.fromList [(l, []) | l <- (Map.keys . value) tr]
    go acc Trie{..} cs = case cs of
                           [] -> acc'
                           (c:cs') -> case c `Map.lookup` edges of
                                        Nothing -> acc'
                                        Just tr' -> go acc' tr' cs'
      where
        lvalue = Map.map (\x -> [x]) value
        acc' = Map.unionWith (\a b -> a ++ b) acc lvalue


-- |
--
fromSequences :: (Ord l, Ord a, Hashable l, Hashable a, Show l, Show a) => Int -> [(Integer, l, [a])] -> Trie (Entry a) (Map l Integer)
fromSequences n xs = model
  where
    xs' = map (\(i, l, is) -> [(i, l, x) | x <- revWindows n (replicate (n - 1) Start ++ (map Entry is))]) xs
    model = Trie.labeledSuffixCountTrie (concat xs')


-- |
--
updateFromSequences :: (Ord l, Ord a, Hashable l, Hashable a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> Int -> [(Integer, l, [a])] -> Trie (Entry a) (Map l Integer)
updateFromSequences tr n xs = model
  where
    xs' = map (\(i, l, is) -> [(i, l, x) | x <- revWindows n (replicate (n - 1) Start ++ (map Entry is))]) xs
    model = Trie.updateLabeledSuffixCountTrie tr (concat xs')
