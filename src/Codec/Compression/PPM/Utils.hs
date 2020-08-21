module Codec.Compression.PPM.Utils ( lineToInstance
                                   --, lineToTokenInstance
                                   , revWindows
                                   , accuracy
                                   , microFScore
                                   , macroFScore
                                   , toProb
                                   --, evaluateModel
                                   , formatScores
                                   , postproc
                                   , postprocessTrain
                                   , readFileOrStdin
                                   , readBSFileOrStdin                                   
                                   , readFileOrStdinTwo
                                   , writeFileOrStdout
                                   , writeBSFileOrStdout
                                   ) where
import Prelude hiding (lookup, getContents, readFile, strip, lines, writeFile, Word(..), drop)
--import Prelude hiding (drop)
import System.IO (withFile, IOMode(..), stdin, stderr, openFile, stdout, hClose, Handle(..))
import Data.Text.IO (getContents, readFile, hGetContents, hPutStr, writeFile, hPutStrLn)
import Data.Text (Text, strip, lines, stripPrefix, splitOn, pack, unpack, breakOn)
import Codec.Compression.GZip (compress, decompress)
import Data.Text (Text, unpack, drop, breakOn)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShowId)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Codec.Compression.PPM.Element (Element(..))
import qualified Data.ByteString as BSS
import Data.Store
import Data.Hashable (Hashable)
import Control.Monad (join, liftM)
import Text.Printf (printf)
import Data.List (sortOn, maximumBy, intercalate)
import Data.Serialize (encodeLazy, decodeLazy)
import Data.Serialize.Text
import qualified Data.ByteString.Lazy.Char8 as BSC

-- | Calculates accuracy
accuracy :: (Eq a) => [a] -> [a] -> Double
accuracy golds guesses = correct / total
  where
    total = (fromIntegral . length) golds
    correct = (fromIntegral . length) $ [x | (x, y) <- zip golds guesses, x == y]

-- | Calculates micro F-Score
--   Note: the equivalence only holds in the single-classification case
microFScore :: (Eq a) => [a] -> [a] -> Double
microFScore = accuracy

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
--   (label, document) tuple of (Text, [Element]).
lineToInstance :: (Element e) => Text -> Maybe (Text, [e])
lineToInstance l = if l == "" then Nothing else Just $ (label, fromLine $ unpack (drop 1 text))
  where
    (id, rest) = breakOn "\t" l
    (label, text) = breakOn "\t" (drop 1 rest)



-- lineToInstance :: Text -> (Text, [Char])
-- lineToInstance l = (label, unpack (drop 1 text))
--   where
--     (id, rest) = breakOn "\t" l
--     (label, text) = breakOn "\t" (drop 1 rest)

-- -- | Splits a line of format ID<TAB>LABEL<TAB>TEXT into a
-- --   (label, document) tuple of (Text, [String]).
-- lineToTokenInstance :: Text -> (Text, [String])
-- lineToTokenInstance l = (label, words $ unpack (drop 1 text))
--   where
--     (id, rest) = breakOn "\t" l
--     (label, text) = breakOn "\t" (drop 1 rest)

-- -- | Splits a line of format ID<TAB>LABEL<TAB>TEXT into a
-- --   (label, document) tuple of (Text, [Byte]).
-- --   If LABEL is prefixed with a "-", treat as a negative
-- --   label.
-- lineToByteInstance :: Text -> (Text, [Byte])
-- lineToByteInstance l = (label, BS.unpack ((encodeUtf8 . drop 1) text))
--   where
--     (id, rest) = breakOn "\t" l
--     (label, text) = breakOn "\t" (drop 1 rest)

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


-- evaluateModel :: (Element e, Ord e, Hashable e, Show e) => Model Text e -> Int -> [(Integer, Text, [e])] -> Map Text Double
-- evaluateModel model n xs = macroFScore golds guesses
--   where
--     xs' = [(l, t) | (_, l, t) <- xs]
--     golds = map fst xs'
--     guesses = map (classifySequence model n . snd) xs'

formatScores :: (Element e) => [(Maybe Text, Maybe (Text, [e]), Maybe (Map Text Double))] -> String
formatScores xs = intercalate "\n" (map formatScores' xs)
-- (\(g, (l, t), ps) -> printf "%s\t%s\t%s\t%s" l g (toLine t) (formatProb ps)) xs)
--  where
--    formatProb ps = intercalate " " (map (\(k, v) -> printf "%s=%.6f" k v) (Map.toList ps))

formatScores' ((Just g), (Just (l, t)), (Just ps)) = printf "%s\t%s\t%s\t%s" l g (toLine t) (formatProb ps)
  where
    formatProb ps = intercalate " " (map (\(k, v) -> printf "%s=%.6f" k v) (Map.toList ps))    
formatScores' _ = ""

postproc :: (Element e) => (Text, [e]) -> [(Integer, Text, [e])]
postproc (l, cs) = [(v, l', cs) | (v, l') <- labels]
  where
    labels = [case stripPrefix "-" l' of Nothing -> (1, l'); Just l'' -> (-1, l'') | l' <- splitOn "," l]

postprocessTrain :: (Element e) => [(Text, [e])] -> [(Integer, Text, [e])]
postprocessTrain xs = concat expanded
  where
    expanded = map postproc xs

readFileOrStdin :: Maybe String -> IO Text
readFileOrStdin (Just f) = case suf of "gz" -> (liftM (pack . BSC.unpack . decompress . BSC.pack . unpack) . readFile) f
                                       _ -> readFile f
  where
    suf = (reverse . take 2 . reverse) f
readFileOrStdin Nothing = getContents

readFileOrStdinTwo :: Maybe String -> IO (Text, Text)
readFileOrStdinTwo f = do
  txt <- readFileOrStdin f
  return $ breakOn "\n\n" txt

writeFileOrStdout :: Maybe String -> Text -> IO ()
writeFileOrStdout (Just f) s = case suf of "gz" -> writeFile f ((pack . BSC.unpack . compress . BSC.pack . unpack) s)
                                           _ -> writeFile f s
  where
    suf = (reverse . take 2 . reverse) f
writeFileOrStdout Nothing s = hPutStr stdout s

writeBSFileOrStdout :: Maybe String -> BSS.ByteString -> IO ()
writeBSFileOrStdout (Just f) s = BSS.writeFile f s
writeBSFileOrStdout Nothing s = BSS.hPutStr stdout s

readBSFileOrStdin :: (Store m) => Maybe String -> IO (Either PeekException m)
readBSFileOrStdin (Just f) = decode <$> BSS.readFile f
readBSFileOrStdin Nothing = decode <$> BSS.getContents
