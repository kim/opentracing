{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module OpenTracing.Zipkin.V1.HttpReporter
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

import Control.Lens                 hiding (Context)
import Control.Monad                (unless)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Builder
import Data.Monoid
import Network.HTTP.Client          hiding (port)
import Network.HTTP.Types
import OpenTracing.Log
import OpenTracing.Reporting
import OpenTracing.Span
import OpenTracing.Types
import OpenTracing.Zipkin.Types
import OpenTracing.Zipkin.V1.Thrift


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
    <> "/api/v1/spans"
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
        return rq { requestHeaders = [(hContentType, "application/x-thrift")] }

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
         . thriftEncodeSpans
         . map (toThriftSpan _zoLocalEndpoint _zoLogfmt)
         $ spans
