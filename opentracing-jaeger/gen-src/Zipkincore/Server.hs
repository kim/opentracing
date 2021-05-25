{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-name-shadowing -fno-warn-unused-matches #-}

module Zipkincore.Server where

import  Zipkincore.Types
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

data ZipkinCollector
  = ZipkinCollector { submitZipkinBatch :: (Pinch.Server.Context) -> ((Data.Vector.Vector Span)) -> (Prelude.IO (Data.Vector.Vector Response)) }


zipkinCollector_mkServer :: (ZipkinCollector) -> Pinch.Server.ThriftServer
zipkinCollector_mkServer server = let functions =   Data.HashMap.Strict.fromList ([ ( "submitZipkinBatch"
                                                      , Pinch.Server.CallHandler ((\ctx (SubmitZipkinBatch_Args a) -> Pinch.Internal.RPC.wrap @(SubmitZipkinBatch_Result) (submitZipkinBatch (server) (ctx) (a)))) ) ]) in Pinch.Server.createServer ((\nm -> Data.HashMap.Strict.lookup (nm) (functions)))
