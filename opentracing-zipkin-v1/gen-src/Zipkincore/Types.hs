{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedLists, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-name-shadowing -fno-warn-unused-matches #-}

module Zipkincore.Types where

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

cLIENT_SEND :: Data.Text.Text
cLIENT_SEND = "cs"
cLIENT_RECV :: Data.Text.Text
cLIENT_RECV = "cr"
sERVER_SEND :: Data.Text.Text
sERVER_SEND = "ss"
sERVER_RECV :: Data.Text.Text
sERVER_RECV = "sr"
mESSAGE_SEND :: Data.Text.Text
mESSAGE_SEND = "ms"
mESSAGE_RECV :: Data.Text.Text
mESSAGE_RECV = "mr"
wIRE_SEND :: Data.Text.Text
wIRE_SEND = "ws"
wIRE_RECV :: Data.Text.Text
wIRE_RECV = "wr"
cLIENT_SEND_FRAGMENT :: Data.Text.Text
cLIENT_SEND_FRAGMENT = "csf"
cLIENT_RECV_FRAGMENT :: Data.Text.Text
cLIENT_RECV_FRAGMENT = "crf"
sERVER_SEND_FRAGMENT :: Data.Text.Text
sERVER_SEND_FRAGMENT = "ssf"
sERVER_RECV_FRAGMENT :: Data.Text.Text
sERVER_RECV_FRAGMENT = "srf"
lOCAL_COMPONENT :: Data.Text.Text
lOCAL_COMPONENT = "lc"
cLIENT_ADDR :: Data.Text.Text
cLIENT_ADDR = "ca"
sERVER_ADDR :: Data.Text.Text
sERVER_ADDR = "sa"
mESSAGE_ADDR :: Data.Text.Text
mESSAGE_ADDR = "ma"
data Endpoint
  = Endpoint { endpoint_ipv4 :: Data.Int.Int32, endpoint_port :: Data.Int.Int16, endpoint_service_name :: Data.Text.Text, endpoint_ipv6 :: (Prelude.Maybe Data.ByteString.ByteString) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Endpoint where
  type (Tag Endpoint) = Pinch.TStruct

  pinch (Endpoint endpoint_ipv4 endpoint_port endpoint_service_name endpoint_ipv6) = Pinch.struct ([ (1 Pinch..= endpoint_ipv4), (2 Pinch..= endpoint_port), (3 Pinch..= endpoint_service_name), (4 Pinch.?= endpoint_ipv6) ])

  unpinch value = ((((Prelude.pure (Endpoint) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..: 3)) Prelude.<*> (value Pinch..:? 4))


instance Data.Hashable.Hashable Endpoint where

data Annotation
  = Annotation { annotation_timestamp :: Data.Int.Int64, annotation_value :: Data.Text.Text, annotation_host :: (Prelude.Maybe Endpoint) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Annotation where
  type (Tag Annotation) = Pinch.TStruct

  pinch (Annotation annotation_timestamp annotation_value annotation_host) = Pinch.struct ([ (1 Pinch..= annotation_timestamp), (2 Pinch..= annotation_value), (3 Pinch.?= annotation_host) ])

  unpinch value = (((Prelude.pure (Annotation) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..:? 3))


instance Data.Hashable.Hashable Annotation where

data AnnotationType
  = BOOL
  | BYTES
  | I16
  | I32
  | I64
  | DOUBLE
  | STRING
  deriving (Prelude.Eq, Prelude.Ord, GHC.Generics.Generic, Prelude.Show, Prelude.Bounded)

instance Pinch.Pinchable AnnotationType where
  type (Tag AnnotationType) = Pinch.TEnum

  pinch BOOL = Pinch.pinch ((0 :: Data.Int.Int32))
  pinch BYTES = Pinch.pinch ((1 :: Data.Int.Int32))
  pinch I16 = Pinch.pinch ((2 :: Data.Int.Int32))
  pinch I32 = Pinch.pinch ((3 :: Data.Int.Int32))
  pinch I64 = Pinch.pinch ((4 :: Data.Int.Int32))
  pinch DOUBLE = Pinch.pinch ((5 :: Data.Int.Int32))
  pinch STRING = Pinch.pinch ((6 :: Data.Int.Int32))

  unpinch v = do
    val <- Pinch.unpinch (v)
    case (val :: Data.Int.Int32) of
      0 -> Prelude.pure (BOOL)
      1 -> Prelude.pure (BYTES)
      2 -> Prelude.pure (I16)
      3 -> Prelude.pure (I32)
      4 -> Prelude.pure (I64)
      5 -> Prelude.pure (DOUBLE)
      6 -> Prelude.pure (STRING)
      _ -> Prelude.fail (("Unknown value for type AnnotationType: " Prelude.<> Prelude.show (val)))


instance Prelude.Enum AnnotationType where
  fromEnum BOOL = 0
  fromEnum BYTES = 1
  fromEnum I16 = 2
  fromEnum I32 = 3
  fromEnum I64 = 4
  fromEnum DOUBLE = 5
  fromEnum STRING = 6

  toEnum 0 = BOOL
  toEnum 1 = BYTES
  toEnum 2 = I16
  toEnum 3 = I32
  toEnum 4 = I64
  toEnum 5 = DOUBLE
  toEnum 6 = STRING
  toEnum _ = Prelude.error ("Unknown value for enum AnnotationType.")


instance Data.Hashable.Hashable AnnotationType where

data BinaryAnnotation
  = BinaryAnnotation { binaryAnnotation_key :: Data.Text.Text, binaryAnnotation_value :: Data.ByteString.ByteString, binaryAnnotation_annotation_type :: AnnotationType, binaryAnnotation_host :: (Prelude.Maybe Endpoint) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable BinaryAnnotation where
  type (Tag BinaryAnnotation) = Pinch.TStruct

  pinch (BinaryAnnotation binaryAnnotation_key binaryAnnotation_value binaryAnnotation_annotation_type binaryAnnotation_host) = Pinch.struct ([ (1 Pinch..= binaryAnnotation_key), (2 Pinch..= binaryAnnotation_value), (3 Pinch..= binaryAnnotation_annotation_type), (4 Pinch.?= binaryAnnotation_host) ])

  unpinch value = ((((Prelude.pure (BinaryAnnotation) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 2)) Prelude.<*> (value Pinch..: 3)) Prelude.<*> (value Pinch..:? 4))


instance Data.Hashable.Hashable BinaryAnnotation where

data Span
  = Span { span_trace_id :: Data.Int.Int64, span_name :: Data.Text.Text, span_id :: Data.Int.Int64, span_parent_id :: (Prelude.Maybe Data.Int.Int64), span_annotations :: (Data.Vector.Vector Annotation), span_binary_annotations :: (Data.Vector.Vector BinaryAnnotation), span_debug :: (Prelude.Maybe Prelude.Bool), span_timestamp :: (Prelude.Maybe Data.Int.Int64), span_duration :: (Prelude.Maybe Data.Int.Int64), span_trace_id_high :: (Prelude.Maybe Data.Int.Int64) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Span where
  type (Tag Span) = Pinch.TStruct

  pinch (Span span_trace_id span_name span_id span_parent_id span_annotations span_binary_annotations span_debug span_timestamp span_duration span_trace_id_high) = Pinch.struct ([ (1 Pinch..= span_trace_id), (3 Pinch..= span_name), (4 Pinch..= span_id), (5 Pinch.?= span_parent_id), (6 Pinch..= span_annotations), (8 Pinch..= span_binary_annotations), (9 Pinch.?= span_debug), (10 Pinch.?= span_timestamp), (11 Pinch.?= span_duration), (12 Pinch.?= span_trace_id_high) ])

  unpinch value = ((((((((((Prelude.pure (Span) Prelude.<*> (value Pinch..: 1)) Prelude.<*> (value Pinch..: 3)) Prelude.<*> (value Pinch..: 4)) Prelude.<*> (value Pinch..:? 5)) Prelude.<*> (value Pinch..: 6)) Prelude.<*> (value Pinch..: 8)) Prelude.<*> (value Pinch..:? 9)) Prelude.<*> (value Pinch..:? 10)) Prelude.<*> (value Pinch..:? 11)) Prelude.<*> (value Pinch..:? 12))


instance Data.Hashable.Hashable Span where

data Response
  = Response { response_ok :: Prelude.Bool }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Response where
  type (Tag Response) = Pinch.TStruct

  pinch (Response response_ok) = Pinch.struct ([ (1 Pinch..= response_ok) ])

  unpinch value = (Prelude.pure (Response) Prelude.<*> (value Pinch..: 1))


instance Data.Hashable.Hashable Response where

data SubmitZipkinBatch_Args
  = SubmitZipkinBatch_Args { submitZipkinBatch_Args_spans :: (Data.Vector.Vector Span) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable SubmitZipkinBatch_Args where
  type (Tag SubmitZipkinBatch_Args) = Pinch.TStruct

  pinch (SubmitZipkinBatch_Args submitZipkinBatch_Args_spans) = Pinch.struct ([ (1 Pinch..= submitZipkinBatch_Args_spans) ])

  unpinch value = (Prelude.pure (SubmitZipkinBatch_Args) Prelude.<*> (value Pinch..: 1))


instance Pinch.Internal.RPC.ThriftResult SubmitZipkinBatch_Result where
  type (ResultType SubmitZipkinBatch_Result) = (Data.Vector.Vector Response)

  unwrap (SubmitZipkinBatch_Result_Success x) = Prelude.pure (x)

  wrap m = Control.Exception.catches ((SubmitZipkinBatch_Result_Success Prelude.<$> m)) ([  ])


data SubmitZipkinBatch_Result
  = SubmitZipkinBatch_Result_Success (Data.Vector.Vector Response)
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable SubmitZipkinBatch_Result where
  type (Tag SubmitZipkinBatch_Result) = Pinch.TUnion

  pinch (SubmitZipkinBatch_Result_Success x) = Pinch.union (0) (x)

  unpinch v = (Control.Applicative.empty Control.Applicative.<|> (SubmitZipkinBatch_Result_Success Prelude.<$> (v Pinch..: 0)))

