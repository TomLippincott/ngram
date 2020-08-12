{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Compression.PPM.Server (Server(..)
                                    , textToByteString
                                    , byteStringToText
                                    ) where


import Prelude hiding (lookup, Word(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Sequence ((|>))
import qualified Codec.Compression.PPM.Trie as Trie
import Codec.Compression.PPM.Trie (Trie(..), scoreSequence, Entry(..), Model(..), fromSequences, updateFromSequences)
import Codec.Compression.PPM.Utils (revWindows, accuracy, macroFScore, microFScore, lineToInstance)
import Codec.Compression.PPM.Element (Element(..), Word(..), Byte(..))
import qualified Data.Maybe as Maybe
import Data.List (sortOn, maximumBy)
import Control.Monad (join)
import Debug.Trace (traceShowId)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Yesod hiding (Update)
import Network.HTTP.Types (status200, Status(..), status405)
import Network.Wai (pathInfo, rawPathInfo, responseStatus, responseHeaders, requestHeaders, pathInfo, requestBody,
                     queryString, requestMethod, responseLBS, Response(..), ResponseReceived(..), responseBuilder, Request)
import Data.Aeson.Types (Result(..))
import Data.Aeson.Types
import Data.Aeson (encode, (.:?), decode)
import Data.Aeson.Parser (json)
import Network.Wai.Conduit (sourceRequestBody)
import Data.Conduit (($$), (.|), runConduit)
import Data.Conduit.Attoparsec (sinkParser)
import Yesod.Core.Types
import Network.HTTP.Types (Method)
import Data.Binary.Builder (empty, fromByteString)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import qualified Data.HashMap.Strict as HM
import Data.Scientific (toBoundedInteger)
import qualified Data.Vector as Vector
import Control.DeepSeq (deepseq, force, ($!!), NFData, NFData1)
import qualified Data.Store as Store
import Data.Store (Store(..), PeekException)
import Data.ByteString.UTF8 as BLU

data Server e = Server { _model :: IORef (Model Text e)
                       , _n :: IORef Int
                       , _modelType :: IORef Text
                       }

instance Yesod (Server e)
             
instance (NFData e, Element e, Read e, Eq e, Ord e, Hashable e, Show e, Store (Model Text e)) => YesodDispatch (Server e) where
  yesodDispatch yre req f = do
    let method = requestMethod req
        headers = requestHeaders req
        path@(route:_) = pathInfo req
    case (method, route) of ("GET", "info") -> infoR yre req f
                            ("PUT", "reset") -> resetR yre req f
                            ("PATCH", "train") -> trainR yre req f
                            ("GET", "apply") -> applyR yre req f
                            ("GET", "pull_model") -> pullModelR yre req f
                            ("PUT", "push_model") -> pushModelR yre req f
                            _ -> f (responseBuilder status405 [] empty)

infoR yre req f = do
  model <- (readIORef . _model . yreSite) yre
  n <- (readIORef . _n . yreSite) yre
  modelType <- (readIORef . _modelType . yreSite) yre
  let j = object ["modelType" .= modelType, "n" .= n]
      b = (toStrict . encode) j
  f (responseBuilder status200 [] (fromByteString b))


resetR yre req f = do
  Object value <- runConduit $ sourceRequestBody req .| sinkParser json
  let (Number n) = HM.lookupDefault (Number 3) "n" value
      Just n' = toBoundedInteger n :: Maybe Int
      modelRef = (_model . yreSite) yre
      nRef = (_n . yreSite) yre
  atomicModifyIORef' modelRef (\_ -> (fromSequences n' [], ()))
  atomicModifyIORef' nRef (\_ -> (n', ()))
  f (responseBuilder status200 [] empty)
  
trainR yre req f = do
  n <- (readIORef . _n . yreSite) yre
  Object value <- runConduit $ sourceRequestBody req .| sinkParser json
  let modelRef = (_model . yreSite) yre
      (Array docs) = HM.lookupDefault (Array Vector.empty) "docs" value
      docs' = Vector.toList docs
      docs'' = Maybe.catMaybes $ map oneDoc docs'
  atomicModifyIORef' modelRef (updateModel docs'' n)
  f (responseBuilder status200 [] empty)

oneDoc :: (Element e) => Value -> Maybe (Integer, Text, [e])
oneDoc (Object o) = do
  String label <- HM.lookup "label" o
  String text <- HM.lookup "text" o
  String id <- HM.lookup "id" o
  return $ (1, label, (fromLine . Text.unpack) text)  
oneDoc _ = Nothing

updateModel docs n model = (updateFromSequences model n docs, ())

applyR yre req f = do
  model <- (readIORef . _model . yreSite) yre
  n <- (readIORef . _n . yreSite) yre
  Object value <- runConduit $ sourceRequestBody req .| sinkParser json
  let (Array docs) = HM.lookupDefault (Array Vector.empty) "docs" value
      docs' = Vector.toList docs
      docs'' = Maybe.catMaybes $ map oneDoc docs'
      testInstances = map (\(_, _, x) -> x) docs''
      scores = map (scoreSequence model n) testInstances
      res = map oneResult (zip scores docs'')
      j = object ["results" .= res]
      b = (toStrict . encode) j
  f (responseBuilder status200 [] (fromByteString b))

oneResult :: (Element e) => (Map Text Double, (Integer, Text, [e])) -> Value
oneResult (ps, (i, l, t)) = object ["id" .= i, "label" .= l, "text" .= toLine t, "probabilities" .= ps']
  where
    ps' = probsToValue ps

probsToValue :: Map Text Double -> Value
probsToValue ps = object [l .= v | (l, v) <- Map.toList ps]

pullModelR yre req f = do
  model <- (readIORef . _model . yreSite) yre
  modelType <- (readIORef . _modelType . yreSite) yre
  n <- (readIORef . _n . yreSite) yre
  let state = (byteStringToText . Store.encode) (model, n, modelType)
      j = object ["state" .= state]
      b = (toStrict . encode) j
  f (responseBuilder status200 [] (fromByteString b))

byteStringToText :: BS.ByteString -> ST.Text
byteStringToText = ST.decodeUtf8 . B64.encode

textToByteString :: ST.Text -> BS.ByteString
textToByteString txt =
  case B64.decode . ST.encodeUtf8 $ txt of
    Left err -> error err
    Right bs -> bs

pushModelR :: forall e . (Element e, Ord e, Read e, Store (Model Text e)) => YesodRunnerEnv (Server e) -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
pushModelR yre req f = do
  Object value <- runConduit $ sourceRequestBody req .| sinkParser json
  let modelRef = (_model . yreSite) yre
      nRef = (_n . yreSite) yre
      modelTypeRef = (_modelType . yreSite) yre
      String str = HM.lookupDefault (String "") "state" value
      Right (model, n, modelType) = (Store.decode . textToByteString) str :: Either PeekException (Model Text e, Int, Text)
  atomicModifyIORef' modelRef (\_ -> (model, ()))
  atomicModifyIORef' nRef (\_ -> (n, ()))
  f (responseBuilder status200 [] empty)

instance RenderRoute (Server e) where
  data Route (Server e) = ServerRoute [Text]
    deriving (Show, Eq, Read, Ord)
  renderRoute (ServerRoute x) = (x, [])
