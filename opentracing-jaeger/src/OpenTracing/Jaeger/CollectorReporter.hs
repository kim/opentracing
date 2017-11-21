{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module OpenTracing.Jaeger.CollectorReporter
    ( Options(..)

    , defaultCollectorAddr

    , Env
    , newEnv
    , closeEnv
    , withEnv

    , jaegerCollectorReporter
    )
where

import           Control.Lens              (view)
import           Control.Monad             (void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text                 (Text)
import           Data.Vector               (fromList)
import qualified Jaeger_Types              as Thrift
import           Network.HTTP.Client
import           OpenTracing.Jaeger.Thrift
import           OpenTracing.Reporting
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import           Thrift.Protocol.Binary
import           Thrift.Transport.Empty


type Env = BatchEnv

data Options = Options
    { optManager     :: Manager
    , optServiceName :: Text
    , optServiceTags :: Tags
    , optAddr        :: Addr 'HTTP
    }

defaultCollectorAddr :: Addr 'HTTP
defaultCollectorAddr = HTTPAddr "127.0.0.1" 14268 False

newEnv :: Options -> IO Env
newEnv Options{..} = do
    rq <- mkReq
    newBatchEnv 100 $
        reporter rq optManager (toThriftProcess optServiceName optServiceTags)
  where
    mkReq = do
        rq <- parseRequest
                    $ "http://" <> view addrHostName optAddr
                   <> ":"
                   <> show (view addrPort optAddr)
                   <> "/api/traces?format=jaeger.thrift"
        pure rq { method = "POST", secure = view addrSecure optAddr }


closeEnv :: Env -> IO ()
closeEnv = closeBatchEnv

withEnv :: (MonadIO m, MonadMask m) => Options -> (Env -> m a) -> m a
withEnv opts = bracket (liftIO $ newEnv opts) (liftIO . closeEnv)


jaegerCollectorReporter :: MonadIO m => Env -> FinishedSpan -> m ()
jaegerCollectorReporter = batchReporter


reporter :: Request -> Manager -> Thrift.Process -> [FinishedSpan] -> IO ()
reporter rq mgr proc (fromList -> spans) =
    void $ httpLbs rq { requestBody = body } mgr -- XXX: check response status
  where
    body = RequestBodyLBS . serializeBatch $ toThriftBatch proc spans

    -- nb. collector accepts 'BinaryProtocol', but agent 'CompactProtocol'
    serializeBatch = Thrift.encode_Batch (BinaryProtocol EmptyTransport)
