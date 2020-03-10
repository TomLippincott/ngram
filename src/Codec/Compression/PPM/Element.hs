module Codec.Compression.PPM.Element ( Element(..), Word(..), Byte(..) ) where

import Prelude hiding (Word)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as C8 
import qualified Data.ByteString as B

type Byte = Word8
type Word = [Char]

class Element e where
  fromLine :: String -> [e]
  toLine :: [e] -> String

instance Element Char where
  fromLine = id
  toLine = id
  
instance Element Word where
  fromLine = words
  toLine = unwords
  
instance Element Byte where
  fromLine = (B.unpack . C8.pack)
  toLine = (C8.unpack . B.pack)
