{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module OpenTracing.Zipkin.V1.Thrift
    ( toThriftSpan

    , thriftEncodeSpan
    , thriftEncodeSpans
    )
where

import           Control.Lens
import           Data.Bifunctor
import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy     as Lazy
import           Data.ByteString.Lens
import           Data.Foldable            (foldl', toList)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Int
import qualified Data.IP                  as IP
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Semigroup           ((<>))
import           Data.Text.Lazy.Encoding  (decodeUtf8, encodeUtf8)
import qualified Data.Vector              as Vector
import           OpenTracing.Log
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Time
import           OpenTracing.Types
import           OpenTracing.Zipkin.Types (Endpoint (..))
import qualified Thrift
import           Thrift.Protocol.Binary
import           Thrift.Transport.Empty
import           Thrift.Types
import qualified ZipkinCore_Consts        as Thrift
import           ZipkinCore_Types
    ( Annotation (..)
    , BinaryAnnotation (..)
    , Span (..)
    , endpoint_ipv4
    , endpoint_ipv6
    , endpoint_port
    , endpoint_service_name
    )
import qualified ZipkinCore_Types         as Thrift


toThriftSpan
    :: Endpoint
    -> LogFieldsFormatter
    -> FinishedSpan
    -> Thrift.Span
toThriftSpan (toThriftEndpoint -> loc) logfmt s = Thrift.Span
    { span_trace_id           = view (spanContext . to traceIdLo') s
    , span_trace_id_high      = view (spanContext . to traceIdHi') s
    , span_name               = view (spanOperation . lazy) s
    , span_id                 = view (spanContext . to ctxSpanID') s
    , span_parent_id          = view (spanContext . to ctxParentSpanID') s
    , span_annotations        = annotations
    , span_binary_annotations = binaryAnnotations
    , span_debug              = Nothing
    , span_timestamp          = Just tstart
    , span_duration           = view (spanDuration . to micros . re _Just) s
    }
  where
    tstart = view (spanStart . to micros) s

    (annotations, binaryAnnotations)
        = bimap Vector.fromList Vector.fromList
        . first (<> annFromLogs (view spanLogs s))
        $ annFromTags (view spanTags s)

    annFromTags :: Tags -> ([Thrift.Annotation], [Thrift.BinaryAnnotation])
    annFromTags = perhapsLocal . foldl' go ([],[]) . HashMap.toList . fromTags
      where
        go acc (SpanKind sk) =
            let ann = Thrift.Annotation
                    { annotation_timestamp = tstart
                    , annotation_host      = Just loc
                    , annotation_value     = case sk of
                          RPCClient -> Thrift.cLIENT_SEND
                          RPCServer -> Thrift.sERVER_RECV
                          Producer  -> Thrift.mESSAGE_SEND
                          Consumer  -> Thrift.mESSAGE_RECV
                    }
             in first (ann:) acc

        go acc (k,v) =
            let (anntyp, annval) = toThriftTag v
                ann              = Thrift.BinaryAnnotation
                    { binaryAnnotation_key             = view lazy k
                    , binaryAnnotation_value           = annval
                    , binaryAnnotation_annotation_type = anntyp
                    , binaryAnnotation_host            = Just loc
                    }
             in second (ann:) acc

        -- if we don't have a 'SpanKind', we're supposed to tell Zipkin about us
        -- via a 'BinaryAnnotation'
        perhapsLocal ([],bs) = ([],) . (:bs) $ Thrift.BinaryAnnotation
            { binaryAnnotation_key             = Thrift.lOCAL_COMPONENT
            , binaryAnnotation_value           = encodeUtf8 $ endpoint_service_name loc
            , binaryAnnotation_annotation_type = Thrift.STRING
            , binaryAnnotation_host            = Just loc
            }
        perhapsLocal xs = xs

    annFromLogs :: [LogRecord] -> [Thrift.Annotation]
    annFromLogs = foldl' go []
      where
        go acc (LogRecord t fs) = Thrift.Annotation
            { annotation_timestamp = micros t
            , annotation_host      = Just loc
            , annotation_value     = case fs of
                  (Event x :| []) -> view lazy x -- proper zipkin annotation
                  fields          -> decodeUtf8 . toLazyByteString $ logfmt fields
            }
            : acc

thriftEncodeSpan :: Thrift.Span -> Lazy.ByteString
thriftEncodeSpan = Thrift.encode_Span (BinaryProtocol EmptyTransport)

thriftEncodeSpans :: Traversable t => t Thrift.Span -> Lazy.ByteString
thriftEncodeSpans
    = thriftEncodeVal
    . TList (T_STRUCT Thrift.typemap_Span)
    . toList
    . fmap Thrift.from_Span

thriftEncodeVal :: ThriftVal -> Lazy.ByteString
thriftEncodeVal = Thrift.serializeVal (BinaryProtocol EmptyTransport)

toThriftTag :: TagVal -> (Thrift.AnnotationType, Lazy.ByteString)
toThriftTag (BoolT   v) = (Thrift.BOOL, if v then "1" else "0")
toThriftTag (StringT v) = (Thrift.STRING, view (lazy . to encodeUtf8) v)
toThriftTag (IntT    v) = (Thrift.I64, toLazyByteString . int64BE $ v)
toThriftTag (DoubleT v) = (Thrift.DOUBLE, toLazyByteString . doubleBE $ v)
toThriftTag (BinaryT v) = (Thrift.BYTES, v)

toThriftEndpoint :: Endpoint -> Thrift.Endpoint
toThriftEndpoint Endpoint{..} = Thrift.Endpoint
    { endpoint_ipv4         = packIPv4 $ fromIPv4 ipv4
    , endpoint_port         = maybe 0 (fromIntegral . fromPort) port
    , endpoint_service_name = view lazy serviceName
    , endpoint_ipv6         = packIPv6 . fromIPv6 <$> ipv6
    }
  where
    packIPv4 :: IP.IPv4 -> Int32
    packIPv4 ip =
        let [a,b,c,d] = IP.fromIPv4 ip
         in fromIntegral $ a `shiftL` 24 .|. b `shiftL` 16 .|. c `shiftL` 8 .|. d

    packIPv6 :: IP.IPv6 -> Lazy.ByteString
    packIPv6 = view packedBytes . map fromIntegral . IP.fromIPv6b


traceIdLo' :: SpanContext -> Int64
traceIdLo' = fromIntegral . traceIdLo . ctxTraceID

traceIdHi' :: SpanContext -> Maybe Int64
traceIdHi' = fmap fromIntegral . traceIdHi . ctxTraceID

ctxSpanID' :: SpanContext -> Int64
ctxSpanID' = fromIntegral . ctxSpanID

ctxParentSpanID' :: SpanContext -> Maybe Int64
ctxParentSpanID' = fmap fromIntegral . ctxParentSpanID
