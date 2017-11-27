{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module OpenTracing.Jaeger.AgentReporter
    ( JaegerAgentOptions
    , jaegerAgentOptions
    , jaoServiceName
    , jaoServiceTags
    , jaoAddr
    , jaoErrorLog

    , defaultJaegerAgentAddr

    , JaegerAgent
    , newJaegerAgent
    , closeJaegerAgent
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


data JaegerAgent = JaegerAgent
    { envLocalProcess :: Thrift.Process
    , envErrorLog     :: Builder -> IO ()
    , envTransport    :: AgentTransport
    }

newtype AgentTransport = AgentTransport Handle
    deriving Thrift.Transport

data JaegerAgentOptions = JaegerAgentOptions
    { _jaoServiceName :: Text
    , _jaoServiceTags :: Tags
    , _jaoAddr        :: Addr 'UDP
    , _jaoErrorLog    :: Builder -> IO ()
    }

jaegerAgentOptions :: Text -> JaegerAgentOptions
jaegerAgentOptions srv = JaegerAgentOptions
    { _jaoServiceName = srv
    , _jaoServiceTags = mempty
    , _jaoAddr        = defaultJaegerAgentAddr
    , _jaoErrorLog    = defaultErrorLog
    }

defaultJaegerAgentAddr :: Addr 'UDP
defaultJaegerAgentAddr = UDPAddr "127.0.0.1" 6831


newJaegerAgent :: JaegerAgentOptions -> IO JaegerAgent
newJaegerAgent JaegerAgentOptions{..} =
    let tproc = toThriftProcess _jaoServiceName _jaoServiceTags
     in JaegerAgent tproc _jaoErrorLog <$> openAgentTransport _jaoAddr

closeJaegerAgent :: JaegerAgent -> IO ()
closeJaegerAgent JaegerAgent{envTransport} =
    handleAny (const (return ())) $
        Thrift.tFlush envTransport *> Thrift.tClose envTransport

withJaegerAgent
    :: ( MonadIO   m
       , MonadMask m
       )
    => JaegerAgentOptions
    -> (JaegerAgent -> m a)
    -> m a
withJaegerAgent opts =
    bracket (liftIO $ newJaegerAgent opts) (liftIO . closeJaegerAgent)

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

jaegerAgentReporter :: MonadIO m => JaegerAgent -> FinishedSpan -> m ()
jaegerAgentReporter JaegerAgent{..} s = liftIO $ emit `catchAny` err
  where
    proto = Thrift.CompactProtocol envTransport
    emit  = Thrift.emitBatch (undefined, proto)
          $ toThriftBatch envLocalProcess (Vector.singleton s)
    err e = envErrorLog $ shortByteString "Jaeger Agent Thrift error: "
                       <> string8 (show e)
                       <> char8 '\n'

makeLenses ''JaegerAgentOptions
