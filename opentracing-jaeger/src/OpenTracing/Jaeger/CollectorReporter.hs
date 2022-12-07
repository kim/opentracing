{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module OpenTracing.Jaeger.CollectorReporter
    ( JaegerCollectorOptions
    , jaegerCollectorOptions
    , jcoManager
    , jcoServiceName
    , jcoServiceTags
    , jcoAddr
    , jcoErrorLog

    , defaultJaegerCollectorAddr

    , JaegerCollector
    , newJaegerCollector
    , closeJaegerCollector
    , withJaegerCollector

    , jaegerCollectorReporter

    , jaegerPropagation

    , newManager
    , defaultManagerSettings
    )
where

import           Control.Lens                   (makeLenses, set, view)
import           Control.Monad                  (unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString.Builder
import           Data.ByteString.Lazy           (fromStrict)
import           Data.Text                      (Text)
import           Data.Vector                    (fromList)
import qualified Jaeger.Types                   as Thrift
import           Network.HTTP.Client
import           Network.HTTP.Types             (hContentType)
import           Network.HTTP.Types.Status
import           OpenTracing.Jaeger.Propagation (jaegerPropagation)
import           OpenTracing.Jaeger.Thrift
import           OpenTracing.Reporting
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import qualified Pinch

newtype JaegerCollector = JaegerCollector { fromJaegerCollector :: BatchEnv }

data JaegerCollectorOptions = JaegerCollectorOptions
    { _jcoManager     :: Manager
    , _jcoServiceName :: Text
    , _jcoServiceTags :: Tags
    , _jcoAddr        :: Addr 'HTTP
    , _jcoErrorLog    :: Builder -> IO ()
    }

makeLenses ''JaegerCollectorOptions

jaegerCollectorOptions :: Manager -> Text -> JaegerCollectorOptions
jaegerCollectorOptions mgr srv = JaegerCollectorOptions
    { _jcoManager     = mgr
    , _jcoServiceName = srv
    , _jcoServiceTags = mempty
    , _jcoAddr        = defaultJaegerCollectorAddr
    , _jcoErrorLog    = defaultErrorLog
    }

defaultJaegerCollectorAddr :: Addr 'HTTP
defaultJaegerCollectorAddr = HTTPAddr "127.0.0.1" 14268 False

newJaegerCollector :: JaegerCollectorOptions -> IO JaegerCollector
newJaegerCollector opt@JaegerCollectorOptions{..} = do
    rq <- mkReq
    fmap JaegerCollector
        . newBatchEnv
        . set boptErrorLog _jcoErrorLog . batchOptions
        $ reporter _jcoManager _jcoErrorLog rq tproc
  where
    mkReq = do
        rq <- parseRequest
                    $ "http://" <> view (jcoAddr . addrHostName) opt
                   <> ":"
                   <> show (view (jcoAddr . addrPort) opt)
                   <> "/api/traces?format=jaeger.thrift"
        pure rq
            { method = "POST"
            , secure = view (jcoAddr . addrSecure) opt
            , requestHeaders = [(hContentType, "application/x-thrift")]
            }

    tproc = toThriftProcess _jcoServiceName _jcoServiceTags


closeJaegerCollector :: JaegerCollector -> IO ()
closeJaegerCollector = closeBatchEnv . fromJaegerCollector

withJaegerCollector
    :: ( MonadIO   m
       , MonadMask m
       )
    => JaegerCollectorOptions
    -> (JaegerCollector -> m a)
    -> m a
withJaegerCollector opts =
    bracket (liftIO $ newJaegerCollector opts) (liftIO . closeJaegerCollector)


jaegerCollectorReporter :: MonadIO m => JaegerCollector -> FinishedSpan -> m ()
jaegerCollectorReporter = batchReporter . fromJaegerCollector


reporter
    :: Manager
    -> (Builder -> IO ())
    -> Request
    -> Thrift.Process
    -> [FinishedSpan]
    -> IO ()
reporter mgr errlog rq tproc (fromList -> spans) = do
    rs <- responseStatus <$> httpNoBody rq { requestBody = body } mgr
    unless (statusIsSuccessful rs) $
        errlog $ shortByteString "Error from Jaeger Collector: "
              <> intDec (statusCode rs)
              <> char8 '\n'
  where
    body = RequestBodyBS . serializeBatch $ toThriftBatch tproc spans

    -- nb. collector accepts 'BinaryProtocol', but agent 'CompactProtocol'
    serializeBatch = Pinch.encode Pinch.binaryProtocol
