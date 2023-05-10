{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches #-}

module Agent.Server where

import  Agent.Types
import qualified Pinch.Server
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

data Agent
  = Agent { emitZipkinBatch :: (Pinch.Server.Context) -> ((Data.Vector.Vector Zipkincore.Types.Span)) -> (Prelude.IO ()), emitBatch :: (Pinch.Server.Context) -> (Jaeger.Types.Batch) -> (Prelude.IO ()) }


agent_mkServer :: (Agent) -> Pinch.Server.ThriftServer
agent_mkServer server = let functions =   Data.HashMap.Strict.fromList ([ ( "emitZipkinBatch"
                                            , Pinch.Server.OnewayHandler ((\ctx (EmitZipkinBatch_Args a) -> emitZipkinBatch (server) (ctx) (a))) ), ( "emitBatch"
                                            , Pinch.Server.OnewayHandler ((\ctx (EmitBatch_Args a) -> emitBatch (server) (ctx) (a))) ) ]) in Pinch.Server.createServer ((\nm -> Data.HashMap.Strict.lookup (nm) (functions)))
