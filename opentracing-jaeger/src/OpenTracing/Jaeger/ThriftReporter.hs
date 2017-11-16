{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}

module OpenTracing.Jaeger.ThriftReporter where

import qualified Agent_Client                   as Thrift
import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Reader
import           Data.Bool                      (bool)
import           Data.Foldable
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import           Data.Text.Lens
import qualified Data.Vector                    as Vector
import           Data.Vector.Lens               (vector)
import           GHC.Stack                      (prettyCallStack)
import           Jaeger_Types
    (Span (..), Tag (..), batch_spans)
import qualified Jaeger_Types                   as Thrift
import           Network.Socket
import           Network.Socket.ByteString.Lazy (sendAll)
import           OpenTracing.Log
import           OpenTracing.Span
import           OpenTracing.Tags               (TagVal (..), fromTags)
import           OpenTracing.Time
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


openUDPTransport :: IO UDP
openUDPTransport = do
    addr : _ <- getAddrInfo (Just defaultHints { addrSocketType = Datagram })
                            (Just "127.0.0.1")
                            (Just "6831")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)

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

toThriftSpan :: FinishedSpan -> Thrift.Span
toThriftSpan s = Thrift.Span
    { span_traceIdLow    = view (spanContext . to traceIdLo') s
    , span_traceIdHigh   = view (spanContext . to traceIdHi') s
    , span_spanId        = view (spanContext . to ctxSpanID') s
    , span_parentSpanId  = maybe 0 (ctxSpanID' . refCtx)
                         . findParent
                         $ view spanRefs s
    , span_operationName = view (spanOperation . lazy) s
    , span_references    = view ( spanRefs
                                . to (map toThriftSpanRef . toList)
                                . vector
                                . re _Just
                                )
                                s
    , span_flags         = view ( spanContext
                                . ctxSampled
                                . re _IsSampled
                                . to (bool 0 1)
                                )
                                s
    , span_startTime     = view (spanStart . to micros) s
    , span_duration      = view (spanDuration . to micros) s
    , span_tags          = Just
                         . ifoldMap (\k v -> Vector.singleton (toThriftTag k v))
                         . fromTags
                         $ view spanTags s
    , span_logs          = Just
                         . Vector.fromList
                         . foldr' (\r acc -> toThriftLog r : acc) []
                         $ view spanLogs s
    }

toThriftSpanRef :: Reference -> Thrift.SpanRef
toThriftSpanRef ref = Thrift.SpanRef
    { spanRef_refType     = toThriftRefType ref
    , spanRef_traceIdLow  = traceIdLo' (refCtx ref)
    , spanRef_traceIdHigh = traceIdHi' (refCtx ref)
    , spanRef_spanId      = ctxSpanID' (refCtx ref)
    }

toThriftRefType :: Reference -> Thrift.SpanRefType
toThriftRefType (ChildOf     _) = Thrift.CHILD_OF
toThriftRefType (FollowsFrom _) = Thrift.FOLLOWS_FROM

toThriftTag :: Text -> TagVal -> Thrift.Tag
toThriftTag k v =
    let t = Thrift.default_Tag { tag_key = view lazy k }
     in case v of
            BoolT   x -> t { tag_vBool   = Just x }
            StringT x -> t { tag_vStr    = Just (view lazy x) }
            IntT    x -> t { tag_vLong   = Just x }
            DoubleT x -> t { tag_vDouble = Just x }
            BinaryT x -> t { tag_vBinary = Just x }

toThriftLog :: LogRecord -> Thrift.Log
toThriftLog r = Thrift.Log
    { log_timestamp = view (logTime . to micros) r
    , log_fields    = foldMap ( Vector.singleton
                              . uncurry toThriftTag
                              . asTag
                              )
                    $ view logFields r
    }
  where
    asTag f = (logFieldLabel f,) . StringT $ case f of
        LogField _ v -> view packed (show v)
        Event      v -> v
        Message    v -> v
        Stack      v -> view packed (prettyCallStack v)
        ErrKind    v -> v
        ErrObj     v -> view packed (show v)

traceIdLo' :: SpanContext -> Int64
traceIdLo' = fromIntegral . traceIdLo . ctxTraceID

traceIdHi' :: SpanContext -> Int64
traceIdHi' = maybe 0 fromIntegral . traceIdHi . ctxTraceID

ctxSpanID' :: SpanContext -> Int64
ctxSpanID' = fromIntegral . ctxSpanID
