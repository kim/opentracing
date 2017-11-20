{-# LANGUAGE TupleSections #-}

module OpenTracing.Jaeger.Thrift
    ( toThriftSpan
    , toThriftTags
    , toThriftProcess
    , toThriftBatch
    )
where

import           Control.Lens
import           Data.Bool         (bool)
import           Data.Foldable
import           Data.Int          (Int64)
import           Data.Text         (Text)
import           Data.Text.Lens
import           Data.Vector       (Vector)
import qualified Data.Vector       as Vector
import           Data.Vector.Lens  (vector)
import           GHC.Stack         (prettyCallStack)
import           Jaeger_Types
    ( Batch (..)
    , Log (..)
    , Process (..)
    , Span (..)
    , SpanRef (..)
    , Tag (..)
    )
import qualified Jaeger_Types      as Thrift
import           OpenTracing.Log
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Time
import           OpenTracing.Types (TraceID (..))


toThriftSpan :: FinishedSpan -> Thrift.Span
toThriftSpan s = Thrift.Span
    { span_traceIdLow    = view (spanContext . to traceIdLo') s
    , span_traceIdHigh   = view (spanContext . to traceIdHi') s
    , span_spanId        = view (spanContext . to ctxSpanID') s
    , span_parentSpanId  = maybe 0 (ctxSpanID' . refCtx) . findParent
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
    , span_tags          = view (spanTags . to toThriftTags . re _Just) s
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

toThriftTags :: Tags -> Vector Thrift.Tag
toThriftTags = ifoldMap (\k v -> Vector.singleton (toThriftTag k v)) . fromTags

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

toThriftProcess :: Text -> Tags -> Thrift.Process
toThriftProcess srv tags = Thrift.Process
    { process_serviceName = view lazy srv
    , process_tags        = Just $ toThriftTags tags
    }

toThriftBatch :: Thrift.Process -> Vector FinishedSpan -> Thrift.Batch
toThriftBatch proc spans = Thrift.Batch
    { batch_process = proc
    , batch_spans   = toThriftSpan <$> spans
    }

traceIdLo' :: SpanContext -> Int64
traceIdLo' = fromIntegral . traceIdLo . ctxTraceID

traceIdHi' :: SpanContext -> Int64
traceIdHi' = maybe 0 fromIntegral . traceIdHi . ctxTraceID

ctxSpanID' :: SpanContext -> Int64
ctxSpanID' = fromIntegral . ctxSpanID
