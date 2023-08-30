{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module OpenTracing.Zipkin.V2.HttpReporter
    ( ZipkinOptions
    , zipkinOptions
    , zoManager
    , zoLocalEndpoint
    , zoEndpoint
    , zoLogfmt
    , zoErrorLog

    , defaultZipkinEndpoint
    , defaultZipkinAddr

    , Zipkin
    , newZipkin
    , closeZipkin
    , withZipkin

    , zipkinHttpReporter

    , Endpoint(..)

    , newManager
    , defaultManagerSettings
    )
where

import           Control.Lens                hiding (Context)
import           Control.Monad               (unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson                  hiding (Error)
import           Data.Aeson.Encoding
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Builder
import           Data.Map.Lens               (toMapOf)
import           Data.Maybe                  (catMaybes)
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Data.Text.Strict.Lens       (packed, utf8)
import           Network.HTTP.Client
import           Network.HTTP.Types
import           OpenTracing.Log
import           OpenTracing.Reporting
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Time
import           OpenTracing.Types
import           OpenTracing.Zipkin.Types

newtype Zipkin = Zipkin { fromZipkin :: BatchEnv }

data ZipkinOptions = ZipkinOptions
    { _zoManager       :: Manager
    , _zoLocalEndpoint :: Endpoint
    , _zoEndpoint      :: String
    , _zoLogfmt        :: forall t. Foldable t => t LogField -> Builder -- == LogFieldsFormatter
    , _zoErrorLog      :: Builder -> IO ()
    }

makeLenses ''ZipkinOptions

zipkinOptions :: Manager -> Endpoint -> ZipkinOptions
zipkinOptions mgr loc = ZipkinOptions
    { _zoManager       = mgr
    , _zoLocalEndpoint = loc
    , _zoEndpoint      = defaultZipkinEndpoint
    , _zoLogfmt        = jsonMap
    , _zoErrorLog      = defaultErrorLog
    }

defaultZipkinEndpoint :: String
defaultZipkinEndpoint = "http://"
    <> view addrHostName addr
    <> ":"
    <> show (view addrPort addr)
    <> "/api/v2/spans"
  where
    addr = defaultZipkinAddr

newZipkin :: ZipkinOptions -> IO Zipkin
newZipkin opts@ZipkinOptions{_zoEndpoint=endpoint, _zoErrorLog=errlog} = do
    rq <- mkReq
    fmap Zipkin
        . newBatchEnv
        . set boptErrorLog errlog . batchOptions
        $ reporter opts rq
  where
    mkReq = do
        rq <- parseRequest endpoint
        return rq { method = "POST", requestHeaders = [(hContentType, "application/json")] }

closeZipkin :: Zipkin -> IO ()
closeZipkin = closeBatchEnv . fromZipkin

withZipkin
    :: ( MonadIO   m
       , MonadMask m
       )
    => ZipkinOptions
    -> (Zipkin -> m a)
    -> m a
withZipkin opts = bracket (liftIO $ newZipkin opts) (liftIO . closeZipkin)


zipkinHttpReporter :: MonadIO m => Zipkin -> FinishedSpan -> m ()
zipkinHttpReporter = batchReporter . fromZipkin

reporter :: ZipkinOptions -> Request -> [FinishedSpan] -> IO ()
reporter ZipkinOptions{..} rq spans = do
    rs <- responseStatus <$> httpLbs rq { requestBody = body } _zoManager
    unless (statusIsSuccessful rs) $
        _zoErrorLog $ shortByteString "Error from Zipkin server: "
                    <> intDec (statusCode rs)
                    <> char8 '\n'
  where
    body = RequestBodyLBS
         . encodingToLazyByteString
         . list (spanE _zoLocalEndpoint _zoLogfmt)
         $ spans


spanE :: Endpoint -> LogFieldsFormatter -> FinishedSpan -> Encoding
spanE loc logfmt s = pairs $
       pair "name"           (view (spanOperation . to text) s)
    <> pair "id"             (view (spanContext . to ctxSpanID  . hexText . to text) s)
    <> pair "traceId"        (view (spanContext . to ctxTraceID . hexText . to text) s)
    <> maybe mempty
            (pair "parentId" . text . view hexText)
            (view (spanContext . to ctxParentSpanID) s)
    <> maybe mempty
            (pair "kind" . toEncoding)
            (view (spanTags . to (getTag SpanKindKey)) s)
    <> pair "timestamp"      (view (spanStart . to microsE) s)
    <> pair "duration"       (view (spanDuration . to microsE) s)
    <> pair "debug"          (bool False)
    <> pair "localEndpoint"  (toEncoding loc)
    <> maybe mempty
             (pair "remoteEndpoint")
             (view (spanTags . to remoteEndpoint) s)
    <> pair "annotations"    (list (logRecE logfmt) $ view spanLogs s)
    -- Zipkin V2 requires tag values to be strings
    <> pair "tags"           (toEncoding . toMapOf (spanTags . to fromTags . ifolded . to tagToText) $ s)
    -- nb. references are lost, perhaps we should stick them into annotations?
  where tagToText = \ case
          BoolT b   -> view (to show . packed) b
          StringT t -> t
          IntT i    -> view (to show . packed) i
          DoubleT d -> view (to show . packed) d
          BinaryT b -> view (to B64.encode . strict . utf8) b

remoteEndpoint :: Tags -> Maybe Encoding
remoteEndpoint ts = case fields of
    [] -> Nothing
    xs -> Just . pairs $ mconcat xs
  where
    fields = catMaybes
        [ pair PeerServiceKey . toEncoding <$> getTag PeerServiceKey ts
        , pair PeerIPv4Key    . toEncoding <$> getTag PeerIPv4Key    ts
        , pair PeerIPv6Key    . toEncoding <$> getTag PeerIPv6Key    ts
        , pair PeerPortKey    . toEncoding <$> getTag PeerPortKey    ts
        ]

logRecE :: LogFieldsFormatter -> LogRecord -> Encoding
logRecE logfmt r = pairs $
       pair "timestamp" (view (logTime . to microsE) r)
    <> pair "value"     (lazyText . decodeUtf8 . toLazyByteString . logfmt $ view logFields r)

microsE :: AsMicros a => a -> Encoding
microsE = word64 . micros
