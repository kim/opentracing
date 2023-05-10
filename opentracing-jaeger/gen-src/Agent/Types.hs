{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches #-}

module Agent.Types where

import qualified Jaeger.Types
import qualified Zipkincore.Types
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

data EmitZipkinBatch_Args
  = EmitZipkinBatch_Args { emitZipkinBatch_Args_spans :: (Data.Vector.Vector Zipkincore.Types.Span) }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable EmitZipkinBatch_Args where
  type (Tag EmitZipkinBatch_Args) = Pinch.TStruct

  pinch (EmitZipkinBatch_Args emitZipkinBatch_Args_spans) = Pinch.struct ([ (1 Pinch..= emitZipkinBatch_Args_spans) ])

  unpinch value = (Prelude.pure (EmitZipkinBatch_Args) Prelude.<*> (value Pinch..: 1))


data EmitBatch_Args
  = EmitBatch_Args { emitBatch_Args_batch :: Jaeger.Types.Batch }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable EmitBatch_Args where
  type (Tag EmitBatch_Args) = Pinch.TStruct

  pinch (EmitBatch_Args emitBatch_Args_batch) = Pinch.struct ([ (1 Pinch..= emitBatch_Args_batch) ])

  unpinch value = (Prelude.pure (EmitBatch_Args) Prelude.<*> (value Pinch..: 1))
