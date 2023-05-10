{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches #-}

module Jaeger.Server where

import  Jaeger.Types
import qualified Pinch.Server
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

data Collector
  = Collector { submitBatches :: (Pinch.Server.Context) -> ((Data.Vector.Vector Batch)) -> (Prelude.IO (Data.Vector.Vector BatchSubmitResponse)) }


collector_mkServer :: (Collector) -> Pinch.Server.ThriftServer
collector_mkServer server = let functions =   Data.HashMap.Strict.fromList ([ ( "submitBatches"
                                                , Pinch.Server.CallHandler ((\ctx (SubmitBatches_Args a) -> Pinch.Internal.RPC.wrap @(SubmitBatches_Result) (submitBatches (server) (ctx) (a)))) ) ]) in Pinch.Server.createServer ((\nm -> Data.HashMap.Strict.lookup (nm) (functions)))
