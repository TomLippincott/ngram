{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Codec.Compression.PPM.Trie ( Trie(..)
                                  , Context(..)
                                  , lookup
                                  --, labeledSuffixCountTrie
                                  , emptyTrie
                                  , updateLabeledSuffixCountTrie
                                  , fromSequences
                                  , scoreSequence
                                  , rectifyModel
                                  , scoreGram
                                  , classifySequence
                                  , updateFromSequences
                                  , toCounts
                                  , Entry(..)
                                  , Model(..)
                                  ) where

import Prelude hiding (lookup, Word)
import Data.Text (Text)
import Data.Bits
import Control.Monad (join, liftM)
import qualified Data.List as L
import Data.Foldable (toList, foldl')
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic, Generic1)
import TH.Derive
import Data.Store
import Data.Hashable (Hashable)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Codec.Compression.PPM.Utils (revWindows, accuracy, macroFScore, microFScore, lineToInstance, toProb)
import Codec.Compression.PPM.Element (Byte(..), Word(..), Element(..))
import Data.List (sortOn, maximumBy)
import Debug.Trace (traceShowId)
import Data.Store
import Control.DeepSeq (deepseq, force, ($!!), NFData, NFData1)

data Entry a = Entry a | Start deriving (Show, Read, Ord, Eq, Generic)
instance Hashable a => Hashable (Entry a)
instance (Serialize a, Ord a) => Serialize (Entry a)
type Model l a = Trie (Entry a) (Map l Integer)

-- | Trie nodes may have an optional arbitrary value, and each edge is
--   associated with a particular value seen in the input sequences.
data Trie e v = Trie { value :: v
                     , edges :: Map e (Trie e v)
                     } deriving (Show, Read, Generic, Eq)

instance (NFData e) => NFData (Entry e)
instance (NFData k, NFData v) => NFData (Trie k v)

data Context v c = Context Int

addSequenceWithLabel :: (Ord l, Ord e, Hashable l, Hashable e, Show l, Show e) => Trie e (Map l Integer) -> (Integer, l, [e]) -> Trie e (Map l Integer)
addSequenceWithLabel tr items = case items of (i, l, []) -> Trie { value=Map.insertWith (+) l i (value tr)
                                                                 , edges=edges tr
                                                                 }
                                              (i, l, (x:xs)) -> Trie { value=value'
                                                                     , edges=edges''
                                                                     }
                                                where
                                                  edges' = edges tr
                                                  old = Map.findWithDefault (Trie Map.empty Map.empty) x edges'
                                                  edges'' = Map.insert x (addSequenceWithLabel old (i, l, xs)) edges'
                                                  value' = Map.insertWith (+) l i (value tr)


--labeledSuffixCountTrie :: (Ord l, Ord e, NFData e, NFData l, Hashable l, Hashable e, Show l, Show e) => [(Integer, l, [e])] -> Trie e (Map l Integer)
--labeledSuffixCountTrie xs = force $ foldl' addSequenceWithLabel (Trie Map.empty Map.empty) xs


updateLabeledSuffixCountTrie :: (Ord l, Ord e, NFData l, NFData e, Hashable l, Hashable e, Show l, Show e) => Trie e (Map l Integer) -> [(Integer, l, [e])] -> Trie e (Map l Integer)
updateLabeledSuffixCountTrie tr !xs = force $ foldl' addSequenceWithLabel tr xs

emptyTrie = Trie Map.empty Map.empty

lookup :: (Ord e, Hashable e) => [e] -> Trie e v -> Maybe (Trie e v)
lookup [] tr = Just tr
lookup (e:es) (Trie {..}) = join $ lookup es <$> (e `Map.lookup` edges)

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
fromSequences :: (Ord l, Ord a, NFData a, NFData l, Hashable l, Hashable a, Show l, Show a) => Int -> [(Integer, l, [a])] -> Trie (Entry a) (Map l Integer)
fromSequences n xs = updateLabeledSuffixCountTrie model (concat xs')
  where
    xs' = map (\(i, l, is) -> [(i, l, x) | x <- revWindows n (replicate (n - 1) Start ++ (map Entry is))]) xs
    model = Trie Map.empty Map.empty
    --model = labeledSuffixCountTrie (concat xs')


-- |
--
updateFromSequences :: (Ord l, Ord a, Hashable l, NFData l, NFData a, Hashable a, Show l, Show a) => Trie (Entry a) (Map l Integer) -> Int -> [(Integer, l, [a])] -> Trie (Entry a) (Map l Integer)
updateFromSequences tr n xs = model
  where
    xs' = map (\(i, l, is) -> [(i, l, x) | x <- revWindows n (replicate (n - 1) Start ++ (map Entry is))]) xs
    model = updateLabeledSuffixCountTrie tr (concat xs')

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



$($(derive [d|
   instance Deriving (Store (Entry Char))
   |]))

$($(derive [d|
   instance Deriving (Store (Entry Word))
   |]))

$($(derive [d|
   instance Deriving (Store (Entry Byte))
   |]))

$($(derive [d|
   instance Deriving (Store (Trie (Entry Char) (Map Text Integer)))
   |]))

$($(derive [d|
   instance Deriving (Store (Trie (Entry Word) (Map Text Integer)))
   |]))

$($(derive [d|
   instance Deriving (Store (Trie (Entry Byte) (Map Text Integer)))
   |]))
