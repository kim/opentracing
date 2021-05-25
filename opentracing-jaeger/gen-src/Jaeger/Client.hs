{-# LANGUAGE TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-name-shadowing -fno-warn-unused-matches #-}

module Jaeger.Client where

import  Jaeger.Types
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

submitBatches :: ((Data.Vector.Vector Batch)) -> (Pinch.Client.ThriftCall SubmitBatches_Result)
submitBatches batches = Pinch.Client.TCall ("submitBatches") (SubmitBatches_Args (batches))
