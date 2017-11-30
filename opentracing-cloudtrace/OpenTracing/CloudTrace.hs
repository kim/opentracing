{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module OpenTracing.CloudTrace
    ( ProjectId
    , CloudTraceScopes

    , CloudTraceOptions
    , cloudTraceOptions
    , simpleCloudTraceOptions
    , ctoProjectId
    , ctoManager
    , ctoLogger
    , ctoCredentials

    , CloudTrace
    , newCloudTrace
    , closeCloudTrace
    , withCloudTrace

    , cloudTraceReporter
    )
where

import           Control.Lens
import           Control.Monad             (void)
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
import           Data.Text                 (Text)
import qualified Data.Text.Lazy.Encoding   as LT
import           Data.Time.Clock           (addUTCTime)
import           Network.Google
import           Network.Google.Auth
import           Network.Google.CloudTrace
import           Network.HTTP.Client       (Manager)
import           OpenTracing.Reporting
import           OpenTracing.Span
import           OpenTracing.Tags          hiding (Error)
import           OpenTracing.Types


type ProjectId = Text

type CloudTraceScopes =
    '[ "https://www.googleapis.com/auth/trace.append"
     , "https://www.googleapis.com/auth/cloud-platform"
     ]

newtype CloudTrace = CloudTrace { fromCloudTrace :: BatchEnv }

data CloudTraceOptions = CloudTraceOptions
    { _ctoProjectId   :: Text
    , _ctoManager     :: Manager
    , _ctoLogger      :: Logger
    , _ctoCredentials :: Credentials CloudTraceScopes
    }

makeLenses ''CloudTraceOptions

cloudTraceOptions
    :: ProjectId
    -> Manager
    -> Credentials CloudTraceScopes
    -> CloudTraceOptions
cloudTraceOptions pid mgr creds = CloudTraceOptions
    { _ctoProjectId   = pid
    , _ctoManager     = mgr
    , _ctoLogger      = logger
    , _ctoCredentials = creds
    }
  where
    logger Error = defaultErrorLog
    logger _     = const $ pure ()


simpleCloudTraceOptions :: ProjectId -> IO CloudTraceOptions
simpleCloudTraceOptions pid = do
    m <- newManager tlsManagerSettings
    c <- getApplicationDefault m
    return $ cloudTraceOptions pid m c


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
reporter CloudTraceOptions{_ctoProjectId=pid} goog spans =
    request $ projectsPatchTraces (set tTraces traces' traces) pid
  where
    request = void . runResourceT . runGoogle goog . send
    traces' = mapMaybe (fmap (toGoogTrace pid) . nonEmpty)
            . groupBy ((==) `on` view (spanContext . to ctxTraceID))
            $ spans


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
    zulu = encTxt . utcTime
    kind = toGoogSpanKind . getTagReify _SpanKind SpanKindKey
    endt = addUTCTime (view spanDuration s) (view spanStart s)

toGoogTags :: Tags -> TraceSpanLabels
toGoogTags = traceSpanLabels . HashMap.foldlWithKey' tr mempty . fromTags
  where
    -- TODO: map logs here somehow?
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
