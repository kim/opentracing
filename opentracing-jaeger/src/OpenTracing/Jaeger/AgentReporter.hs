{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}

module OpenTracing.Jaeger.AgentReporter
    ( Env
    , newEnv
    , closeEnv
    , withEnv

    , AgentAddr(..)
    , defaultAgentAddr

    , jaegerAgentReporter
    )
where

import qualified Agent_Client                   as Thrift
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Text                      (Text)
import qualified Data.Vector                    as Vector
import           Jaeger_Types                   (batch_process, batch_spans)
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
    { envServiceName :: Text
    , envProcTags    :: Tags
    , envTransport   :: UDP
    }

data UDP = UDP
    { udpSock :: Socket
    , udpWBuf :: Thrift.WriteBuffer
    , udpRBuf :: Thrift.ReadBuffer
    }

instance Thrift.Transport UDP where
    tIsOpen        = isConnected . udpSock
    tClose         = close . udpSock
    tRead          = Thrift.readBuf  . udpRBuf
    tWrite         = Thrift.writeBuf . udpWBuf
    tPeek          = Thrift.peekBuf  . udpRBuf
    tFlush UDP{..} = Thrift.flushBuf udpWBuf >>= sendAll udpSock

data AgentAddr = AgentAddr
    { agentHost :: HostName
    , agentPort :: Port
    }

defaultAgentAddr :: AgentAddr
defaultAgentAddr = AgentAddr
    { agentHost = "127.0.0.1"
    , agentPort = 6831
    }

newEnv :: Text -> Tags -> AgentAddr -> IO Env
newEnv srv tags addr = Env srv tags <$> openUDP addr

closeEnv :: Env -> IO ()
closeEnv Env{envTransport} =
    Thrift.tFlush envTransport *> Thrift.tClose envTransport

withEnv
    :: ( MonadIO   m
       , MonadMask m
       )
    => Text
    -> Tags
    -> AgentAddr
    -> (Env -> m a)
    -> m a
withEnv srv tags addr =
    bracket (liftIO $ newEnv srv tags addr) (liftIO . closeEnv)

openUDP :: AgentAddr -> IO UDP
openUDP AgentAddr{agentHost,agentPort} = do
    AddrInfo{..} : _ <- getAddrInfo (Just defaultHints { addrSocketType = Datagram })
                                    (Just agentHost)
                                    (Just (show agentPort))
    sock <- socket addrFamily addrSocketType addrProtocol
    connect sock addrAddress

    UDP sock <$> Thrift.newWriteBuffer <*> Thrift.newReadBuffer


jaegerAgentReporter :: MonadIO m => Env -> FinishedSpan -> m ()
jaegerAgentReporter Env{..} s = liftIO $
    Thrift.emitBatch (undefined, proto) Thrift.default_Batch
        { batch_spans   = Vector.singleton $ toThriftSpan s
        , batch_process = Thrift.Process
            { process_serviceName = view lazy envServiceName
            , process_tags        = Just $ toThriftTags envProcTags
            }
        }
  where
    proto = Thrift.CompactProtocol envTransport
