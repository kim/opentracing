{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module OpenTracing.CloudTrace
    ( ProjectId
    , CloudTraceScopes

    , CloudTraceOptions
    , cloudTraceOptions
    , simpleCloudTraceOptions
    , ctoProjectId
    , ctoResource
    , ctoManager
    , ctoLogger
    , ctoCredentials

    , CloudTrace
    , newCloudTrace
    , closeCloudTrace
    , withCloudTrace

    , cloudTraceReporter

    , newManager
    , tlsManagerSettings
    )
where

import           Control.Lens
import           Control.Monad             (unless, void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson                (toEncoding)
import           Data.Aeson.Encoding
import           Data.Function             (on)
import qualified Data.HashMap.Strict       as HashMap
import           Data.List                 (groupBy)
import           Data.List.NonEmpty        (NonEmpty (..), nonEmpty)
import           Data.Maybe                (mapMaybe)
import           Data.Semigroup
import           Data.Text                 (Text, dropAround)
import qualified Data.Text.Lazy.Encoding   as LT
import           Data.Time.Clock           (addUTCTime)
import           Network.Google
import           Network.Google.Auth
import           Network.Google.CloudTrace
import           Network.Google.Logging
import           Network.HTTP.Client       (Manager)
import           OpenTracing.Log
import           OpenTracing.Reporting
import           OpenTracing.Span
import           OpenTracing.Tags          hiding (Error)
import           OpenTracing.Types


type ProjectId = Text

type CloudTraceScopes =
    '[ "https://www.googleapis.com/auth/trace.append"
     , "https://www.googleapis.com/auth/logging.write"
     , "https://www.googleapis.com/auth/cloud-platform"
     ]

newtype CloudTrace = CloudTrace { fromCloudTrace :: BatchEnv }

data CloudTraceOptions = CloudTraceOptions
    { _ctoProjectId   :: Text
    , _ctoResource    :: MonitoredResource
    , _ctoManager     :: Manager
    , _ctoLogger      :: Logger
    , _ctoCredentials :: Credentials CloudTraceScopes
    }

makeLenses ''CloudTraceOptions

cloudTraceOptions
    :: ProjectId
    -> MonitoredResource
    -> Manager
    -> Credentials CloudTraceScopes
    -> CloudTraceOptions
cloudTraceOptions pid res mgr creds = CloudTraceOptions
    { _ctoProjectId   = pid
    , _ctoResource    = res
    , _ctoManager     = mgr
    , _ctoLogger      = logger
    , _ctoCredentials = creds
    }
  where
    logger Error = defaultErrorLog
    logger _     = const $ pure ()


simpleCloudTraceOptions
    :: ProjectId
    -> Maybe MonitoredResource
    -> IO CloudTraceOptions
simpleCloudTraceOptions pid Nothing = simpleCloudTraceOptions pid $
      Just
    . set mrType   (Just "global")
    . set mrLabels ( Just . monitoredResourceLabels
                   $ HashMap.singleton "project_id" pid
                   )
    $ monitoredResource
simpleCloudTraceOptions pid (Just res) = do
    m <- newManager tlsManagerSettings
    c <- getApplicationDefault m
    return $ cloudTraceOptions pid res m c


newCloudTrace :: CloudTraceOptions -> IO CloudTrace
newCloudTrace opts@CloudTraceOptions{..} = do
    goog <- newEnvWith _ctoCredentials _ctoLogger _ctoManager
    fmap CloudTrace
        . newBatchEnv
        . set boptErrorLog (_ctoLogger Error) . batchOptions
        $ reporter opts goog

closeCloudTrace :: CloudTrace -> IO ()
closeCloudTrace = closeBatchEnv . fromCloudTrace

withCloudTrace
    :: ( MonadIO   m
       , MonadMask m
       )
    => CloudTraceOptions
    -> (CloudTrace -> m a)
    -> m a
withCloudTrace opts =
    bracket (liftIO $ newCloudTrace opts) (liftIO . closeCloudTrace)


cloudTraceReporter :: MonadIO m => CloudTrace -> FinishedSpan -> m ()
cloudTraceReporter = batchReporter . fromCloudTrace


reporter :: CloudTraceOptions -> Env CloudTraceScopes -> [FinishedSpan] -> IO ()
reporter CloudTraceOptions{_ctoProjectId=pid,_ctoResource=res} goog spans = do
    request $ projectsPatchTraces (set tTraces traces' traces) pid
    unless (null logs) $
        request $ entriesWrite (logrq logs)
  where
    request :: (HasScope CloudTraceScopes a, GoogleRequest a) => a -> IO ()
    request = void . runResourceT . runGoogle goog . send

    traces' = mapMaybe (fmap (toGoogTrace pid) . nonEmpty)
            . groupBy ((==) `on` view (spanContext . to ctxTraceID))
            $ spans

    logs    = concatMap (toGoogLogEntries pid res) spans
    logrq e = set wlerEntries        e
            . set wlerPartialSuccess (Just True)
            . set wlerLogName        (Just $ "projects/" <> pid <> "/logs/tracing")
            $ writeLogEntriesRequest


toGoogLogEntries :: ProjectId -> MonitoredResource -> FinishedSpan -> [LogEntry]
toGoogLogEntries p r s = map mkEntry (view spanLogs s)
  where
    traceId = "projects/" <> p <> "/traces/"
           <> view (spanContext . to ctxTraceID . hexText) s

#if MIN_VERSION_gogol_logging(0,3,1)
    spanId  = view (spanContext . to ctxSpanID . hexText) s
#endif

    mkEntry lr
        = set leTrace       (Just traceId)
#if MIN_VERSION_gogol_logging(0,3,1)
        . set leSpanId      (Just spanId)
#endif
        . set leTimestamp   (view (logTime . re _Just) lr)
        . set leResource    (Just r)
        . set leJSONPayload (Just $ json lr)
        $ logEntry

    json
        = logEntryJSONPayload
        . foldMap
            (\lf -> HashMap.singleton (logFieldLabel lf) (logFieldValue lf))
        . view logFields

toGoogTrace :: ProjectId -> NonEmpty FinishedSpan -> Trace
toGoogTrace p (s :| ss)
    = set tTraceId   (view (spanContext . to ctxTraceID . hexText . re _Just) s)
    . set tSpans     (map toGoogSpan (s : ss))
    . set tProjectId (Just p)
    $ trace

toGoogSpan :: FinishedSpan -> TraceSpan
toGoogSpan s
    = set tsStartTime    (view (spanStart . to zulu . re _Just) s)
    . set tsKind         (view (spanTags  . to kind . re _Just) s)
    . set tsName         (view (spanOperation . re _Just) s)
    . set tsEndTime      (Just $ zulu endt)
    . set tsLabels       (view (spanTags . to toGoogTags . re _Just) s)
    . set tsParentSpanId (view (spanContext . to ctxParentSpanID) s)
    . set tsSpanId       (view (spanContext . to ctxSpanID . re _Just) s)
    $ traceSpan
  where
    zulu = unquote . encTxt . utcTime
    kind = toGoogSpanKind . getTagReify _SpanKind SpanKindKey
    endt = addUTCTime (view spanDuration s) (view spanStart s)

    -- unfortunately, 'utcTime' quotes the result
    unquote = dropAround (=='"')

toGoogTags :: Tags -> TraceSpanLabels
toGoogTags = traceSpanLabels . HashMap.foldlWithKey' tr mempty . fromTags
  where
    tr m ComponentKey      v = HashMap.insert "/component"        (val v) m
    tr m HttpMethodKey     v = HashMap.insert "/http/method"      (val v) m
    tr m HttpStatusCodeKey v = HashMap.insert "/http/status_code" (val v) m
    tr m HttpUrlKey        v = HashMap.insert "/http/url"         (val v) m
    tr m k                 v = HashMap.insert (ns k)              (val v) m

    ns  = ("opentracing.io/" <>)
    val = encTxt . toEncoding

toGoogSpanKind :: Maybe SpanKinds -> Text
toGoogSpanKind (Just RPCClient) = "RPC_CLIENT"
toGoogSpanKind (Just RPCServer) = "RPC_SERVER"
toGoogSpanKind _                = "SPAN_KIND_UNSPECIFIED"

encTxt :: Encoding -> Text
encTxt = view strict . LT.decodeUtf8 . encodingToLazyByteString
