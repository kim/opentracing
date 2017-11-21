{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}

module OpenTracing.Jaeger.AgentReporter
    ( Options(..)

    , defaultAgentAddr

    , Env
    , newEnv
    , closeEnv
    , withEnv

    , jaegerAgentReporter
    )
where

import qualified Agent_Client                   as Thrift
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Text                      (Text)
import qualified Data.Vector                    as Vector
import qualified Jaeger_Types                   as Thrift
import           Network.Socket
import           Network.Socket.ByteString.Lazy (sendAll)
import           OpenTracing.Jaeger.Thrift
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import           Prelude                        hiding (span)
import qualified Thrift
import qualified Thrift.Protocol.Compact        as Thrift
import qualified Thrift.Transport.IOBuffer      as Thrift


data Env = Env
    { envLocalProcess :: Thrift.Process
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
    { optServiceName :: Text
    , optServiceTags :: Tags
    , optAddr        :: Addr 'UDP
    }

defaultAgentAddr :: Addr 'UDP
defaultAgentAddr = UDPAddr "127.0.0.1" 6831


newEnv :: Options -> IO Env
newEnv Options{..} =
    let tproc = toThriftProcess optServiceName optServiceTags
     in Env tproc <$> openUDPTrans optAddr

closeEnv :: Env -> IO ()
closeEnv Env{envTransport} =
    Thrift.tFlush envTransport *> Thrift.tClose envTransport

withEnv :: (MonadIO m, MonadMask m) => Options -> (Env -> m a) -> m a
withEnv opts = bracket (liftIO $ newEnv opts) (liftIO . closeEnv)

openUDPTrans :: Addr 'UDP -> IO UDPTrans
openUDPTrans (UDPAddr agentHost agentPort) = do
    AddrInfo{..} : _ <- getAddrInfo (Just defaultHints { addrSocketType = Datagram })
                                    (Just agentHost)
                                    (Just (show agentPort))
    sock <- socket addrFamily addrSocketType addrProtocol
    connect sock addrAddress

    UDPTrans sock <$> Thrift.newWriteBuffer <*> Thrift.newReadBuffer


jaegerAgentReporter :: MonadIO m => Env -> FinishedSpan -> m ()
jaegerAgentReporter Env{envLocalProcess,envTransport} s =
    liftIO . Thrift.emitBatch (undefined, proto) $
        toThriftBatch envLocalProcess (Vector.singleton s)
  where
    proto = Thrift.CompactProtocol envTransport
