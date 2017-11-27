{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module OpenTracing.Jaeger.AgentReporter
    ( Options
    , jaegerAgentOptions
    , optServiceName
    , optServiceTags
    , optAddr
    , optErrorLog

    , defaultJaegerAgentAddr

    , Env
    , newJaegerAgentEnv
    , closeJaegerAgentEnv
    , withJaegerAgent

    , jaegerAgentReporter

    , jaegerPropagation
    )
where

import qualified Agent_Client                   as Thrift
import           Control.Exception.Safe
import           Control.Lens                   (makeLenses, view)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Builder
import           Data.Semigroup
import           Data.Text                      (Text)
import qualified Data.Vector                    as Vector
import qualified Jaeger_Types                   as Thrift
import           Network.Socket
import           OpenTracing.Jaeger.Propagation (jaegerPropagation)
import           OpenTracing.Jaeger.Thrift
import           OpenTracing.Reporting          (defaultErrorLog)
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import           Prelude                        hiding (span)
import           System.IO
    ( BufferMode (..)
    , Handle
    , IOMode (..)
    , hSetBinaryMode
    , hSetBuffering
    )
import qualified Thrift
import qualified Thrift.Protocol.Compact        as Thrift
import           Thrift.Transport.Handle        ()


data Env = Env
    { envLocalProcess :: Thrift.Process
    , envErrorLog     :: Builder -> IO ()
    , envTransport    :: AgentTransport
    }

newtype AgentTransport = AgentTransport Handle
    deriving Thrift.Transport

data Options = Options
    { _optServiceName :: Text
    , _optServiceTags :: Tags
    , _optAddr        :: Addr 'UDP
    , _optErrorLog    :: Builder -> IO ()
    }

jaegerAgentOptions :: Text -> Options
jaegerAgentOptions srv = Options
    { _optServiceName = srv
    , _optServiceTags = mempty
    , _optAddr        = defaultJaegerAgentAddr
    , _optErrorLog    = defaultErrorLog
    }

defaultJaegerAgentAddr :: Addr 'UDP
defaultJaegerAgentAddr = UDPAddr "127.0.0.1" 6831


newJaegerAgentEnv :: Options -> IO Env
newJaegerAgentEnv Options{..} =
    let tproc = toThriftProcess _optServiceName _optServiceTags
     in Env tproc _optErrorLog <$> openAgentTransport _optAddr

closeJaegerAgentEnv :: Env -> IO ()
closeJaegerAgentEnv Env{envTransport} = handleAny (const (return ())) $
    Thrift.tFlush envTransport *> Thrift.tClose envTransport

withJaegerAgent :: (MonadIO m, MonadMask m) => Options -> (Env -> m a) -> m a
withJaegerAgent opts =
    bracket (liftIO $ newJaegerAgentEnv opts) (liftIO . closeJaegerAgentEnv)

openAgentTransport :: Addr 'UDP -> IO AgentTransport
openAgentTransport addr = do
    AddrInfo{..} : _ <- getAddrInfo (Just defaultHints { addrSocketType = Datagram })
                                    (Just . view addrHostName $ addr)
                                    (Just . show . view addrPort $ addr)
    sock <- socket addrFamily addrSocketType addrProtocol
    connect sock addrAddress
    hdl  <- socketToHandle sock ReadWriteMode
    hSetBuffering  hdl NoBuffering -- MUST flush after every message!
    hSetBinaryMode hdl True

    return $ AgentTransport hdl

jaegerAgentReporter :: MonadIO m => Env -> FinishedSpan -> m ()
jaegerAgentReporter Env{..} s = liftIO $ emit `catchAny` err
  where
    proto = Thrift.CompactProtocol envTransport
    emit  = Thrift.emitBatch (undefined, proto)
          $ toThriftBatch envLocalProcess (Vector.singleton s)
    err e = envErrorLog $ shortByteString "Jaeger Agent Thrift error: "
                       <> string8 (show e)
                       <> char8 '\n'

makeLenses ''Options
