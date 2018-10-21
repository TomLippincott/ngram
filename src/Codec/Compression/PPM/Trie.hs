{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Codec.Compression.PPM.Trie ( Trie(..)
                                  , Context(..)
                                  , lookup
                                  , labeledSuffixCountTrie
                                  ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Bits
import Control.Monad (join, liftM)
import qualified Data.List as L
import Data.Foldable (toList)
import qualified Data.Maybe as Maybe
import Data.Serialize (Serialize)
import GHC.Generics (Generic)


-- | Trie nodes may have an optional arbitrary value, and each edge is
--   associated with a particular value seen in the input sequences.
data Trie e v = Trie { value :: v
                     , edges :: Map e (Trie e v)
                     } deriving (Show, Read, Generic)


instance (Serialize e, Serialize v, Ord e, Ord v) => Serialize (Trie e v)

data Context v c = Context Int


addSequenceWithLabel :: (Ord l, Ord e) => Trie e (Map l Integer) -> (l, [e]) -> Trie e (Map l Integer)
addSequenceWithLabel (Trie{..}) (l, []) = Trie { value=value'
                                               , edges=edges
                                               }
  where
    value' = Map.insertWith (+) l 1 value

addSequenceWithLabel (Trie{..}) (l, (x:xs)) = Trie { value=value'
                                                   , edges=edges'
                                                   }
  where
    old = Map.findWithDefault (Trie Map.empty Map.empty) x edges
    edges' = Map.insert x (addSequenceWithLabel old (l, xs)) edges
    value' = Map.insertWith (+) l 1 value
    

labeledSuffixCountTrie :: (Ord l, Ord e) => [(l, [e])] -> Trie e (Map l Integer)
labeledSuffixCountTrie xs = foldl addSequenceWithLabel (Trie Map.empty Map.empty) xs


lookup :: (Ord e) => [e] -> Trie e v -> Maybe (Trie e v)
lookup [] tr = Just tr
lookup (e:es) (Trie {..}) = join $ lookup es <$> (edges Map.!? e)
