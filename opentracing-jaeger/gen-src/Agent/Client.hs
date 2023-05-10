{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches #-}

module Agent.Client where

import  Agent.Types
import qualified Pinch.Client
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

emitZipkinBatch :: ((Data.Vector.Vector Zipkincore.Types.Span)) -> (Pinch.Client.ThriftCall ())
emitZipkinBatch spans = Pinch.Client.TOneway ("emitZipkinBatch") (EmitZipkinBatch_Args (spans))

emitBatch :: (Jaeger.Types.Batch) -> (Pinch.Client.ThriftCall ())
emitBatch batch = Pinch.Client.TOneway ("emitBatch") (EmitBatch_Args (batch))
