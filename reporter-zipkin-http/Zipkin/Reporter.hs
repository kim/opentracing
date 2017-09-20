{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Zipkin.Reporter (traceReport) where

import           Control.Lens           hiding (Context)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson             (ToJSON (toEncoding))
import           Data.Aeson.Encoding
import           Data.Foldable
import qualified Data.HashSet           as HashSet
import           Data.Monoid
import           Data.Set               (Set, lookupLT)
import qualified Data.Text              as Text
import           Data.Text.Lens         (packed)
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Stack              (prettyCallStack)
import           Network.HTTP.Client    (RequestBody (..), httpLbs, requestBody)
import           Network.HTTP.Types     (statusCode)
import           OpenTracing.Types
import           Zipkin.Reporter.Config
import           Zipkin.Tracer


-- todo: this should happen asynchronously / batched
traceReport :: MonadIO m => FinishedSpan Context -> Tracer m ()
traceReport s = do
    Config{..} <- asks envReporterConfig
    void . liftIO $
        httpLbs cfgReq { requestBody = body cfgLocalEndpoint } cfgMgr
  where
    body loc = RequestBodyLBS $ encodingToLazyByteString $ list (spanE loc) [s]


spanE :: Endpoint -> FinishedSpan Context -> Encoding
spanE loc s = pairs $
       pair "name"           (view (spanOperation . to text) s)
    <> pair "id"             (view (spanContext . to ctxSpanID  . re _ID . to text) s)
    <> pair "traceId"        (view (spanContext . to ctxTraceID . re _ID . to text) s)
    <> maybe mempty
            (pair "parentid" . text . review _ID)
            (view (spanContext . to ctxParentSpanID) s)
    <> maybe mempty
            (pair "kind" . text . spanKindLabel)
            (view (spanTags . to spanKind) s)
    <> pair "timestamp"      (view (spanStart . to utcTimeToPOSIXSeconds . to micros . to word64) s)
    <> pair "duration"       (view (spanDuration . to micros . to word64) s)
    <> pair "debug"          (bool . HashSet.member Debug $ view (spanContext . ctxFlags) s)
    <> pair "localEndpoint"  (toEncoding loc)
    <> pair "remoteEndpoint" (view (spanTags . to remoteEndpoint . to toEncoding) s)
    <> pair "annotations"    (list logRecE $ view spanLogs s)
    <> pair "tags"           (list tagE . toList $ view spanTags s)
    -- nb. references are lost, perhaps we should stick them into annotations?


spanKind :: Set Tag -> Maybe SpanKinds
spanKind ts = lookupLT (SomeTag mempty mempty) ts >>= \case
    SpanKind x -> pure x
    _          -> Nothing

remoteEndpoint :: Set Tag -> Endpoint
remoteEndpoint = foldl' matchTag (Endpoint Nothing Nothing Nothing Nothing)
  where
    matchTag ep (PeerService s) = ep { serviceName = Just s  }
    matchTag ep (PeerIPv4   ip) = ep { ipv4        = Just ip }
    matchTag ep (PeerIPv6   ip) = ep { ipv6        = Just ip }
    matchTag ep (PeerPort    p) = ep { port        = Just p  }

    matchTag ep _               = ep

logRecE :: LogRecord -> Encoding
logRecE r = pairs $
       pair "timestamp" (view (logTime . to utcTimeToPOSIXSeconds . to micros . to word64) r)
    <> pair "value"     (text . Text.intercalate " " . toList . fmap field $ view logFields r)
  where
    field f = logFieldLabel f <> "=" <> case f of
        Event      x -> x
        Message    x -> x
        Stack      x -> view packed . prettyCallStack $ x
        ErrKind    x -> x
        LogField _ x -> view packed (show x)

tagE :: Tag -> Encoding
tagE t = pairs . pair (tagLabel t) $ case t of
    Component             x -> text x
    DbInstance            x -> text x
    DbStatement           x -> text x
    DbType                x -> text x
    DbUser                x -> text x
    Error                 x -> bool x
    HttpMethod            x -> string . show $ x
    HttpStatusCode        x -> int . statusCode $ x
    HttpUrl               x -> text x
    MessageBusDestination x -> text x
    PeerAddress           x -> text x
    PeerHostname          x -> text x
    PeerIPv4              x -> string . show $ x
    PeerIPv6              x -> string . show $ x
    PeerPort              x -> word64 . fromIntegral $ x
    PeerService           x -> text x
    SamplingPriority      x -> word8 x
    SpanKind              x -> text (spanKindLabel x)
    SomeTag             _ x -> text x

micros :: POSIXTime -> Word64
micros = round . (1000000 *)
