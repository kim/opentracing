{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module OpenTracing.Zipkin.Thrift
    ( toThriftSpan
    )
where

import           Control.Lens
import           Data.Bifunctor
import           Data.Bits
import           Data.ByteString.Builder  (toLazyByteString)
import qualified Data.ByteString.Lazy     as Lazy
import           Data.ByteString.Lens
import           Data.Foldable            (foldl')
import           Data.HashMap.Strict      (toList)
import           Data.Int
import qualified Data.IP                  as IP
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Semigroup           ((<>))
import           Data.Text.Lazy.Encoding  (decodeUtf8, encodeUtf8)
import           Data.Time.Clock.POSIX    (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector              as Vector
import           OpenTracing.Log
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import           OpenTracing.Zipkin
    (Flag (Debug), ZipkinContext (..), hasFlag)
import           OpenTracing.Zipkin.Types (Endpoint (..))
import qualified Thrift
import           Thrift.Protocol.Binary
import           Thrift.Transport.Empty
import           Thrift.Types
import qualified ZipkinCore_Consts        as Thrift
import qualified ZipkinCore_Types         as Thrift


toThriftSpan
    :: Endpoint
    -> LogFieldsFormatter
    -> FinishedSpan ZipkinContext
    -> Thrift.Span
toThriftSpan (toThriftEndpoint -> loc) logfmt s = Thrift.Span
    { Thrift.span_trace_id           = view (spanContext . to ctxTraceID . to traceIdLo . to fromIntegral) s
    , Thrift.span_trace_id_high      = view (spanContext . to ctxTraceID . to traceIdHi . to (fmap fromIntegral)) s
    , Thrift.span_name               = view (spanOperation . lazy) s
    , Thrift.span_id                 = view (spanContext . to ctxSpanID . to fromIntegral) s
    , Thrift.span_parent_id          = view (spanContext . to ctxParentSpanID . to (fmap fromIntegral)) s
    , Thrift.span_annotations        = annotations
    , Thrift.span_binary_annotations = binaryAnnotations
    , Thrift.span_debug              = view (spanContext . to (hasFlag Debug) . re _Just) s
    , Thrift.span_timestamp          = Just tstart
    , Thrift.span_duration           = view (spanDuration . to micros . re _Just) s
    }
  where
    tstart = view (spanStart . to utcTimeToPOSIXSeconds . to micros) s

    (annotations, binaryAnnotations)
        = bimap Vector.fromList Vector.fromList
        . first (<> annFromLogs (view spanLogs s))
        $ annFromTags (view spanTags s)

    annFromTags :: Tags -> ([Thrift.Annotation], [Thrift.BinaryAnnotation])
    annFromTags = foldl' go ([],[]) . toList . fromTags
      where
        go acc (SpanKind sk) =
            let ann = Thrift.Annotation
                    { Thrift.annotation_timestamp = tstart
                    , Thrift.annotation_host      = Just loc
                    , Thrift.annotation_value     = case sk of
                          RPCClient -> Thrift.cLIENT_SEND
                          RPCServer -> Thrift.sERVER_RECV
                          Producer  -> Thrift.mESSAGE_SEND
                          Consumer  -> Thrift.mESSAGE_RECV
                    }
             in first (ann:) acc

        go acc (k,v) =
            let (anntyp, annval) = toThriftTag v
                ann              = Thrift.BinaryAnnotation
                    { Thrift.binaryAnnotation_key             = view lazy k
                    , Thrift.binaryAnnotation_value           = ser annval
                    , Thrift.binaryAnnotation_annotation_type = anntyp
                    , Thrift.binaryAnnotation_host            = Just loc
                    }
             in second (ann:) acc


        ser = Thrift.serializeVal (BinaryProtocol EmptyTransport)

    annFromLogs :: [LogRecord] -> [Thrift.Annotation]
    annFromLogs = foldl' go []
      where
        go acc (LogRecord t fs) = Thrift.Annotation
            { Thrift.annotation_timestamp = micros . utcTimeToPOSIXSeconds $ t
            , Thrift.annotation_host      = Just loc
            , Thrift.annotation_value     = case fs of
                  (Event x :| []) -> view lazy x -- proper zipkin annotation
                  fields          -> decodeUtf8 . toLazyByteString $ logfmt fields
            }
            : acc

toThriftTag :: TagVal -> (Thrift.AnnotationType, ThriftVal)
toThriftTag (BoolT   v) = (Thrift.BOOL, TBool v)
toThriftTag (StringT v) = (Thrift.STRING, TString (view (lazy . to encodeUtf8) v))
toThriftTag (IntT    v) = (Thrift.I64, TI64 v)
toThriftTag (DoubleT v) = (Thrift.DOUBLE, TDouble v)
toThriftTag (BinaryT v) = (Thrift.BYTES, TString v)

toThriftEndpoint :: Endpoint -> Thrift.Endpoint
toThriftEndpoint Endpoint{..} = Thrift.Endpoint
    { Thrift.endpoint_ipv4         = packIPv4 $ maybe "127.0.0.1" fromIPv4 ipv4
    , Thrift.endpoint_port         = maybe 0 (fromIntegral . fromPort) port
    , Thrift.endpoint_service_name = maybe "unknown" (view lazy) serviceName
    , Thrift.endpoint_ipv6         = packIPv6 . fromIPv6 <$> ipv6
    }
  where
    packIPv4 :: IP.IPv4 -> Int32
    packIPv4 ip =
        let [a,b,c,d] = IP.fromIPv4 ip
         in fromIntegral $ a `shiftL` 24 .|. b `shiftL` 16 .|. c `shiftL` 8 .|. d

    packIPv6 :: IP.IPv6 -> Lazy.ByteString
    packIPv6 = view packedBytes . map fromIntegral . IP.fromIPv6b

micros :: POSIXTime -> Int64
micros = round . (1000000*)
