{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedLists, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches #-}

module Zipkincore.Client where

import  Zipkincore.Types
import qualified Pinch.Client
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

submitZipkinBatch :: ((Data.Vector.Vector Span)) -> (Pinch.Client.ThriftCall SubmitZipkinBatch_Result)
submitZipkinBatch spans = Pinch.Client.TCall ("submitZipkinBatch") (SubmitZipkinBatch_Args (spans))
