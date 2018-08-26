{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Compression.PPM.Coding (-- encode
                                    --, decode
                                    --, probability
                                    --, shiftCommonPrefix
                                    --, bitsToInteger
                                    ) where
import Prelude hiding (subtract, lookup, last)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as M
import Data.Bits
import Control.Monad (join, liftM)
import qualified Data.List as L
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
--import Codec.Compression.PPM.Trie (Trie(..), lookup)
--import Codec.Compression.PPM.Utils (windows)
import Data.Ratio ((%))
import Debug.Trace hiding (trace)

-- first :: Word
-- first = zeroBits `setBit` 0

-- last :: Word
-- last = zeroBits `setBit` i
--   where
--     i = (finiteBitSize first) - 1

-- secondToLast :: Word
-- secondToLast = zeroBits `setBit` i
--   where
--     i = (finiteBitSize first) - 2


-- shiftCommonPrefix :: State -> State
-- shiftCommonPrefix (State {..}) = case lb == hb of
--                                    True -> State { low=low `shiftL` 1, high=high `shiftL` 1 `setBit` 0, underflow=underflow, bits=lb:bits }
--                                    False -> case lsb /= hsb of
--                                      True -> State { low=low, high=high, underflow=underflow, bits=bits }
--                                      False -> State { low=low, high=high, underflow=underflow, bits=bits }                                     
--   where
--     i = (finiteBitSize low) - 1
--     lb = low `testBit` i
--     hb = high `testBit` i
--     lsb = low `testBit` (i - 1)
--     hsb = high `testBit` (i - 1)

--   --where
    
--   --   go l' h' bs = case lb == hb of
--   --                   True -> go (shiftL l' 1 `clearBit` 0) (shiftL h' 1 `setBit` 0) (lb:bs)
--   --                   False -> (bs, l', h')

-- bitsToInteger :: [Bool] -> Integer
-- bitsToInteger = go 0
--   where
--     go i [] = i
--     go i (b:bs) = go ((if b == True then setBit else clearBit) (i `shift` 1) 0) bs

-- data Symbol = Symbol { lowCount :: Integer
--                      , highCount :: Integer
--                      , scale :: Integer
--                      } deriving (Show)

-- data State = State { low :: Word
--                    , high :: Word
--                    , underflow :: Int
--                    , bits :: [Bool]
--                    } deriving (Show)

-- type Code = Trie (Maybe Char) (Integer, Integer)

-- -- -- | Under a given trie and with the current range, return the updated range
-- -- updateRange :: Code -> Word -> Word -> [Maybe Char] -> (Word, Word)
-- -- updateRange tr cl ch i = shiftCommonPrefix l' h'
-- --   where
-- --     range = (ch - cl) + 1
-- --     Just (nl, nh) = case lookup i tr of
-- --       Nothing -> error $ "No entry for: " ++ show es
-- --       Just x -> value x
-- --     Just (_, sc) = value tr
-- --     l' = cl + ((range * nl) `div` sc)
-- --     h' = cl + ((range * nh) `div` sc) - 1

-- trace :: (Show a) => a -> a
-- trace a = a -- traceShow a a

-- trace' :: (Show a) => String -> a -> a
-- trace' s a = a --traceShow (s, a) a

-- -- | 
-- encodeItem :: State -> Symbol -> State
-- encodeItem (State{..}) (Symbol{..}) = shiftCommonPrefix $ State { low=fromIntegral (trace low'), high=fromIntegral (trace high'), underflow=underflow, bits=bits }
--   where
--     range = trace' "range: " $ (fromIntegral $ high - low :: Integer) + 1    
--     l = fromIntegral low
--     low' = l + (range * lowCount) `div` scale
--     high' = l + (range * highCount) `div` scale - 1
--     --(shifted, low'', high'') = shiftCommonPrefix low' high'

-- type Input = [Maybe Char]

-- encode :: Code -> Int -> Input -> [Bool]
-- encode code n xs = (reverse . bits) $ foldl encodeItem iState cs''
--   where
--     iState = State zeroBits (complement zeroBits) 0 []
--     Just (_, top) = value code
--     cs = [lookup i code | i <- windows n xs]
--     cs' = [value tr | Just tr <- cs]
--     cs'' = trace [Symbol l h top | Just (l, h) <- cs']

-- decode :: Trie e v -> Integer -> [e]
-- decode tr i = go tr i []
--   where
--     go tr' i' (Nothing:bs) = M.catMaybes $ reverse bs
--     go tr' i' bs = go tr' i' (Nothing:bs)

-- probability :: Trie e v -> [e] -> Double
-- probability tr xs = 0.5
