{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}

module OpenTracing.Jaeger.ThriftReporter
    ( UDP
    , JaegerEndpoint(..)

    , defaultJaegerEndpoint
    , openUDPTransport
    , jaegerThriftReporter
    )
where

import qualified Agent_Client                   as Thrift
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Reader
import qualified Data.Vector                    as Vector
import           Jaeger_Types                   (batch_spans)
import qualified Jaeger_Types                   as Thrift
import           Network.Socket
import           Network.Socket.ByteString.Lazy (sendAll)
import           OpenTracing.Jaeger.Thrift      (toThriftSpan)
import           OpenTracing.Span
import           OpenTracing.Types
import           Prelude                        hiding (span)
import qualified Thrift
import qualified Thrift.Protocol.Compact        as Thrift
import qualified Thrift.Transport.IOBuffer      as Thrift


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

data JaegerEndpoint = JaegerEndpoint
    { jaegerHost :: HostName
    , jaegerPort :: Port
    }

defaultJaegerEndpoint :: JaegerEndpoint
defaultJaegerEndpoint = JaegerEndpoint
    { jaegerHost = "127.0.0.1"
    , jaegerPort = 6831
    }

openUDPTransport :: JaegerEndpoint -> IO UDP
openUDPTransport JaegerEndpoint{jaegerHost,jaegerPort} = do
    AddrInfo{..} : _ <- getAddrInfo (Just defaultHints { addrSocketType = Datagram })
                                    (Just jaegerHost)
                                    (Just (show jaegerPort))
    sock <- socket addrFamily addrSocketType addrProtocol
    connect sock addrAddress

    UDP sock <$> Thrift.newWriteBuffer <*> Thrift.newReadBuffer



jaegerThriftReporter :: MonadIO m => UDP -> FinishedSpan -> m ()
jaegerThriftReporter r = flip runReaderT r . report


report :: (MonadIO m, MonadReader UDP m) => FinishedSpan -> m ()
report s = do
    proto <- Thrift.CompactProtocol <$> ask
    let spans = Vector.singleton . toThriftSpan $ s
    liftIO $
        Thrift.emitBatch (undefined, proto)
                         Thrift.default_Batch { batch_spans = spans }
