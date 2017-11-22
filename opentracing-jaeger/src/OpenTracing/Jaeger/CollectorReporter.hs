{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module OpenTracing.Jaeger.CollectorReporter
    ( Options
    , jaegerCollectorOptions
    , optManager
    , optServiceName
    , optServiceTags
    , optAddr
    , optErrorLog

    , defaultCollectorAddr

    , Env
    , newEnv
    , closeEnv
    , withEnv

    , jaegerCollectorReporter
    )
where

import           Control.Lens              (makeLenses, set, view)
import           Control.Monad             (unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Text                 (Text)
import           Data.Vector               (fromList)
import qualified Jaeger_Types              as Thrift
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           OpenTracing.Jaeger.Thrift
import           OpenTracing.Reporting
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import           Thrift.Protocol.Binary
import           Thrift.Transport.Empty


type Env = BatchEnv

data Options = Options
    { _optManager     :: Manager
    , _optServiceName :: Text
    , _optServiceTags :: Tags
    , _optAddr        :: Addr 'HTTP
    , _optErrorLog    :: Builder -> IO ()
    }

makeLenses ''Options

jaegerCollectorOptions :: Manager -> Text -> Options
jaegerCollectorOptions mgr srv = Options
    { _optManager     = mgr
    , _optServiceName = srv
    , _optServiceTags = mempty
    , _optAddr        = defaultCollectorAddr
    , _optErrorLog    = defaultErrorLog
    }

defaultCollectorAddr :: Addr 'HTTP
defaultCollectorAddr = HTTPAddr "127.0.0.1" 14268 False

newEnv :: Options -> IO Env
newEnv opt@Options{..} = do
    rq <- mkReq
    newBatchEnv . set boptErrorLog _optErrorLog . batchOptions $
        reporter _optManager _optErrorLog rq tproc
  where
    mkReq = do
        rq <- parseRequest
                    $ "http://" <> view (optAddr . addrHostName) opt
                   <> ":"
                   <> show (view (optAddr . addrPort) opt)
                   <> "/api/traces?format=jaeger.thrift"
        pure rq { method = "POST", secure = view (optAddr . addrSecure) opt }

    tproc = toThriftProcess _optServiceName _optServiceTags


closeEnv :: Env -> IO ()
closeEnv = closeBatchEnv

withEnv :: (MonadIO m, MonadMask m) => Options -> (Env -> m a) -> m a
withEnv opts = bracket (liftIO $ newEnv opts) (liftIO . closeEnv)


jaegerCollectorReporter :: MonadIO m => Env -> FinishedSpan -> m ()
jaegerCollectorReporter = batchReporter


reporter
    :: Manager
    -> (Builder -> IO ())
    -> Request
    -> Thrift.Process
    -> [FinishedSpan]
    -> IO ()
reporter mgr errlog rq tproc (fromList -> spans) = do
    rs <- responseStatus <$> httpLbs rq { requestBody = body } mgr
    unless (statusIsSuccessful rs) $
        errlog $ shortByteString "Error from Jaeger Collector: "
              <> intDec (statusCode rs)
              <> char8 '\n'
  where
    body = RequestBodyLBS . serializeBatch $ toThriftBatch tproc spans

    -- nb. collector accepts 'BinaryProtocol', but agent 'CompactProtocol'
    serializeBatch = Thrift.encode_Batch (BinaryProtocol EmptyTransport)
