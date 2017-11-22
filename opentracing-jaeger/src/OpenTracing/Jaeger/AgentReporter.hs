{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module OpenTracing.Jaeger.AgentReporter
    ( Options
    , jaegerAgentOptions
    , optServiceName
    , optServiceTags
    , optAddr
    , optErrorLog

    , defaultAgentAddr

    , Env
    , newEnv
    , closeEnv
    , withEnv

    , jaegerAgentReporter
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
import           Network.Socket.ByteString.Lazy (sendAll)
import           OpenTracing.Jaeger.Thrift
import           OpenTracing.Reporting          (defaultErrorLog)
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import           Prelude                        hiding (span)
import qualified Thrift
import qualified Thrift.Protocol.Compact        as Thrift
import qualified Thrift.Transport.IOBuffer      as Thrift


data Env = Env
    { envLocalProcess :: Thrift.Process
    , envErrorLog     :: Builder -> IO ()
    , envTransport    :: UDPTrans
    }

data UDPTrans = UDPTrans
    { udpSock :: Socket
    , udpWBuf :: Thrift.WriteBuffer
    , udpRBuf :: Thrift.ReadBuffer
    }

instance Thrift.Transport UDPTrans where
    tIsOpen = isConnected . udpSock
    tClose  = close . udpSock
    tRead   = Thrift.readBuf  . udpRBuf
    tWrite  = Thrift.writeBuf . udpWBuf
    tPeek   = Thrift.peekBuf  . udpRBuf
    tFlush UDPTrans{..} = Thrift.flushBuf udpWBuf >>= sendAll udpSock

data Options = Options
    { _optServiceName :: Text
    , _optServiceTags :: Tags
    , _optAddr        :: Addr 'UDP
    , _optErrorLog    :: Builder -> IO ()
    }

makeLenses ''Options

jaegerAgentOptions :: Text -> Options
jaegerAgentOptions srv = Options
    { _optServiceName = srv
    , _optServiceTags = mempty
    , _optAddr        = defaultAgentAddr
    , _optErrorLog    = defaultErrorLog
    }

defaultAgentAddr :: Addr 'UDP
defaultAgentAddr = UDPAddr "127.0.0.1" 6831


newEnv :: Options -> IO Env
newEnv Options{..} =
    let tproc = toThriftProcess _optServiceName _optServiceTags
     in Env tproc _optErrorLog <$> openUDPTrans _optAddr

closeEnv :: Env -> IO ()
closeEnv Env{envTransport} = handleAny (const (return ())) $
    Thrift.tFlush envTransport *> Thrift.tClose envTransport

withEnv :: (MonadIO m, MonadMask m) => Options -> (Env -> m a) -> m a
withEnv opts = bracket (liftIO $ newEnv opts) (liftIO . closeEnv)

openUDPTrans :: Addr 'UDP -> IO UDPTrans
openUDPTrans addr = do
    AddrInfo{..} : _ <- getAddrInfo (Just defaultHints { addrSocketType = Datagram })
                                    (Just . view addrHostName $ addr)
                                    (Just . show . view addrPort $ addr)
    sock <- socket addrFamily addrSocketType addrProtocol
    connect sock addrAddress

    UDPTrans sock <$> Thrift.newWriteBuffer <*> Thrift.newReadBuffer


jaegerAgentReporter :: MonadIO m => Env -> FinishedSpan -> m ()
jaegerAgentReporter Env{..} s = liftIO $ emit `catchAny` err
  where
    proto = Thrift.CompactProtocol envTransport
    emit  = Thrift.emitBatch (undefined, proto)
          $ toThriftBatch envLocalProcess (Vector.singleton s)
    err e = envErrorLog $ shortByteString "Jaeger Agent Thrift error: "
                       <> string8 (show e)
                       <> char8 '\n'
