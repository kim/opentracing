{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-name-shadowing -fno-warn-unused-matches #-}

module Jaeger.Types where

import qualified Prelude
import qualified Control.Applicative
import qualified Control.Exception
import qualified Pinch
import qualified Pinch.Server
import qualified Pinch.Internal.RPC
import qualified Data.Text
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Vector
import qualified Data.HashMap.Strict
import qualified Data.HashSet
import qualified GHC.Generics
import qualified Data.Hashable
import  Data.Vector.Instances ()

data TagType
  = STRING
  | DOUBLE
  | BOOL
  | LONG
  | BINARY
  deriving (Prelude.Eq, Prelude.Ord, GHC.Generics.Generic, Prelude.Show, Prelude.Bounded)

instance Pinch.Pinchable TagType where
  type (Tag TagType) = Pinch.TEnum

  pinch STRING = Pinch.pinch ((0 :: Data.Int.Int32))
  pinch DOUBLE = Pinch.pinch ((1 :: Data.Int.Int32))
  pinch BOOL = Pinch.pinch ((2 :: Data.Int.Int32))
  pinch LONG = Pinch.pinch ((3 :: Data.Int.Int32))
  pinch BINARY = Pinch.pinch ((4 :: Data.Int.Int32))

  unpinch v = do
    val <- Pinch.unpinch (v)
    case (val :: Data.Int.Int32) of
      0 -> Prelude.pure (STRING)
      1 -> Prelude.pure (DOUBLE)
      2 -> Prelude.pure (BOOL)
      3 -> Prelude.pure (LONG)
      4 -> Prelude.pure (BINARY)
      _ -> Prelude.fail (("Unknown value for type TagType: " Prelude.<> Prelude.show (val)))


instance Prelude.Enum TagType where
  fromEnum STRING = 0
  fromEnum DOUBLE = 1
  fromEnum BOOL = 2
  fromEnum LONG = 3
  fromEnum BINARY = 4

  toEnum 0 = STRING
  toEnum 1 = DOUBLE
  toEnum 2 = BOOL
  toEnum 3 = LONG
  toEnum 4 = BINARY
  toEnum _ = Prelude.error ("Unknown value for enum TagType.")


instance Data.Hashable.Hashable TagType where

data Tag
  = Tag { tag_key :: Data.Text.Text, tag_vType :: TagType, tag_vStr :: (Prelude.Maybe Data.Text.Text), tag_vDouble :: (Prelude.Maybe Prelude.Double), tag_vBool :: (Prelude.Maybe Prelude.Bool), tag_vLong :: (Prelude.Maybe Data.Int.Int64), tag_vBinary :: (Prelude.Maybe Data.ByteString.ByteString) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Tag where
  type (Tag Tag) = Pinch.TStruct

  pinch (Tag tag_key tag_vType tag_vStr tag_vDouble tag_vBool tag_vLong tag_vBinary) = Pinch.struct ([ (1 Pinch..= tag_key), (2 Pinch..= tag_vType), (3 Pinch.?= tag_vStr), (4 Pinch.?= tag_vDouble), (5 Pinch.?= tag_vBool), (6 Pinch.?= tag_vLong), (7 Pinch.?= tag_vBinary) ])

  unpinch value = (((((((Prelude.pure (Tag) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..:? 3)) Prelude.<*> (value Pinch..:? 4)) Prelude.<*> (value Pinch..:? 5)) Prelude.<*> (value Pinch..:? 6)) Prelude.<*> (value Pinch..:? 7))


instance Data.Hashable.Hashable Tag where

data Log
  = Log { log_timestamp :: Data.Int.Int64, log_fields :: (Data.Vector.Vector Tag) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Log where
  type (Tag Log) = Pinch.TStruct

  pinch (Log log_timestamp log_fields) = Pinch.struct ([ (1 Pinch..= log_timestamp), (2 Pinch..= log_fields) ])

  unpinch value = ((Prelude.pure (Log) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2))


instance Data.Hashable.Hashable Log where

data SpanRefType
  = CHILD_OF
  | FOLLOWS_FROM
  deriving (Prelude.Eq, Prelude.Ord, GHC.Generics.Generic, Prelude.Show, Prelude.Bounded)

instance Pinch.Pinchable SpanRefType where
  type (Tag SpanRefType) = Pinch.TEnum

  pinch CHILD_OF = Pinch.pinch ((0 :: Data.Int.Int32))
  pinch FOLLOWS_FROM = Pinch.pinch ((1 :: Data.Int.Int32))

  unpinch v = do
    val <- Pinch.unpinch (v)
    case (val :: Data.Int.Int32) of
      0 -> Prelude.pure (CHILD_OF)
      1 -> Prelude.pure (FOLLOWS_FROM)
      _ -> Prelude.fail (("Unknown value for type SpanRefType: " Prelude.<> Prelude.show (val)))


instance Prelude.Enum SpanRefType where
  fromEnum CHILD_OF = 0
  fromEnum FOLLOWS_FROM = 1

  toEnum 0 = CHILD_OF
  toEnum 1 = FOLLOWS_FROM
  toEnum _ = Prelude.error ("Unknown value for enum SpanRefType.")


instance Data.Hashable.Hashable SpanRefType where

data SpanRef
  = SpanRef { spanRef_refType :: SpanRefType, spanRef_traceIdLow :: Data.Int.Int64, spanRef_traceIdHigh :: Data.Int.Int64, spanRef_spanId :: Data.Int.Int64 }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable SpanRef where
  type (Tag SpanRef) = Pinch.TStruct

  pinch (SpanRef spanRef_refType spanRef_traceIdLow spanRef_traceIdHigh spanRef_spanId) = Pinch.struct ([ (1 Pinch..= spanRef_refType), (2 Pinch..= spanRef_traceIdLow), (3 Pinch..= spanRef_traceIdHigh), (4 Pinch..= spanRef_spanId) ])

  unpinch value = ((((Prelude.pure (SpanRef) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..: 3)) Prelude.<*> (value Pinch..: 4))


instance Data.Hashable.Hashable SpanRef where

data Span
  = Span { span_traceIdLow :: Data.Int.Int64, span_traceIdHigh :: Data.Int.Int64, span_spanId :: Data.Int.Int64, span_parentSpanId :: Data.Int.Int64, span_operationName :: Data.Text.Text, span_references :: (Prelude.Maybe (Data.Vector.Vector SpanRef)), span_flags :: Data.Int.Int32, span_startTime :: Data.Int.Int64, span_duration :: Data.Int.Int64, span_tags :: (Prelude.Maybe (Data.Vector.Vector Tag)), span_logs :: (Prelude.Maybe (Data.Vector.Vector Log)) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Span where
  type (Tag Span) = Pinch.TStruct

  pinch (Span span_traceIdLow span_traceIdHigh span_spanId span_parentSpanId span_operationName span_references span_flags span_startTime span_duration span_tags span_logs) = Pinch.struct ([ (1 Pinch..= span_traceIdLow), (2 Pinch..= span_traceIdHigh), (3 Pinch..= span_spanId), (4 Pinch..= span_parentSpanId), (5 Pinch..= span_operationName), (6 Pinch.?= span_references), (7 Pinch..= span_flags), (8 Pinch..= span_startTime), (9 Pinch..= span_duration), (10 Pinch.?= span_tags), (11 Pinch.?= span_logs) ])

  unpinch value = (((((((((((Prelude.pure (Span) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..: 3)) Prelude.<*> (value Pinch..: 4)) Prelude.<*> (value Pinch..: 5)) Prelude.<*> (value Pinch..:? 6)) Prelude.<*> (value Pinch..: 7)) Prelude.<*> (value Pinch..: 8)) Prelude.<*> (value Pinch..: 9)) Prelude.<*> (value Pinch..:? 10)) Prelude.<*> (value Pinch..:? 11))


instance Data.Hashable.Hashable Span where

data Process
  = Process { process_serviceName :: Data.Text.Text, process_tags :: (Prelude.Maybe (Data.Vector.Vector Tag)) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Process where
  type (Tag Process) = Pinch.TStruct

  pinch (Process process_serviceName process_tags) = Pinch.struct ([ (1 Pinch..= process_serviceName), (2 Pinch.?= process_tags) ])

  unpinch value = ((Prelude.pure (Process) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..:? 2))


instance Data.Hashable.Hashable Process where

data ClientStats
  = ClientStats { clientStats_fullQueueDroppedSpans :: Data.Int.Int64, clientStats_tooLargeDroppedSpans :: Data.Int.Int64, clientStats_failedToEmitSpans :: Data.Int.Int64 }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable ClientStats where
  type (Tag ClientStats) = Pinch.TStruct

  pinch (ClientStats clientStats_fullQueueDroppedSpans clientStats_tooLargeDroppedSpans clientStats_failedToEmitSpans) = Pinch.struct ([ (1 Pinch..= clientStats_fullQueueDroppedSpans), (2 Pinch..= clientStats_tooLargeDroppedSpans), (3 Pinch..= clientStats_failedToEmitSpans) ])

  unpinch value = (((Prelude.pure (ClientStats) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..: 3))


instance Data.Hashable.Hashable ClientStats where

data Batch
  = Batch { batch_process :: Process, batch_spans :: (Data.Vector.Vector Span), batch_seqNo :: (Prelude.Maybe Data.Int.Int64), batch_stats :: (Prelude.Maybe ClientStats) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Batch where
  type (Tag Batch) = Pinch.TStruct

  pinch (Batch batch_process batch_spans batch_seqNo batch_stats) = Pinch.struct ([ (1 Pinch..= batch_process), (2 Pinch..= batch_spans), (3 Pinch.?= batch_seqNo), (4 Pinch.?= batch_stats) ])

  unpinch value = ((((Prelude.pure (Batch) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..:? 3)) Prelude.<*> (value Pinch..:? 4))


instance Data.Hashable.Hashable Batch where

data BatchSubmitResponse
  = BatchSubmitResponse { batchSubmitResponse_ok :: Prelude.Bool }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable BatchSubmitResponse where
  type (Tag BatchSubmitResponse) = Pinch.TStruct

  pinch (BatchSubmitResponse batchSubmitResponse_ok) = Pinch.struct ([ (1 Pinch..= batchSubmitResponse_ok) ])

  unpinch value = (Prelude.pure (BatchSubmitResponse) Prelude.<*> (value Pinch..: 1))


instance Data.Hashable.Hashable BatchSubmitResponse where

data SubmitBatches_Args
  = SubmitBatches_Args { submitBatches_Args_batches :: (Data.Vector.Vector Batch) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable SubmitBatches_Args where
  type (Tag SubmitBatches_Args) = Pinch.TStruct

  pinch (SubmitBatches_Args submitBatches_Args_batches) = Pinch.struct ([ (1 Pinch..= submitBatches_Args_batches) ])

  unpinch value = (Prelude.pure (SubmitBatches_Args) Prelude.<*> (value Pinch..: 1))


instance Pinch.Internal.RPC.ThriftResult SubmitBatches_Result where
  type (ResultType SubmitBatches_Result) = (Data.Vector.Vector BatchSubmitResponse)

  unwrap (SubmitBatches_Result_Success x) = Prelude.pure (x)

  wrap m = Control.Exception.catches ((SubmitBatches_Result_Success Prelude.<$> m)) ([  ])


data SubmitBatches_Result
  = SubmitBatches_Result_Success (Data.Vector.Vector BatchSubmitResponse)
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable SubmitBatches_Result where
  type (Tag SubmitBatches_Result) = Pinch.TUnion

  pinch (SubmitBatches_Result_Success x) = Pinch.union (0) (x)

  unpinch v = (Control.Applicative.empty Control.Applicative.<|> (SubmitBatches_Result_Success Prelude.<$> (v Pinch..: 0)))
