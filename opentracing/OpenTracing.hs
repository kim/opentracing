{-|
Module: OpenTracing

The [OpenTracing spec](https://github.com/opentracing/specification/blob/master/specification.md) defines a platform agnostic approach for distributed tracing. Distributed
tracing gives us insights into how complex programs spread across multiple processes are
performing together.

This package provides a core implementation of the OpenTracing spec. It includes
functionality to

  * Create `Span`s describing application code executions, including `Tag`s and
    `LogRecord`s

  * Serialize and deserialize `SpanContext`s across process boundaries

  * Batch and log `FinishedSpan`s

It does not provide any functionality for consuming `Span`s. There are platform specific
backends (CloudTrace, Zipkin, Jaeger, etc) that are provided in other packages.

-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE StrictData       #-}

module OpenTracing
    ( -- * Distributed tracing
      -- | These are mtl style constraints and runners for working with tracers in
      -- a distributed environment. When traces cross process boundaries (for example
      -- in an RPC call, information about the `SpanContext` needs to be transmitted
      -- from one process to another, so that all `Span`s in the same trace can be
      -- reported in the same trace forest.
      --
      -- To satisfy these constraints, you have to have access to a `Propagation` in
      -- the application environment, which manages serialization and deserialization of
      -- `SpanContext`s.
      HasOpenTracing
    , MonadOpenTracing
    , runOpenTracing

      -- * Local tracing
      -- | If you aren't tracing a distributed system, these simpler constraints
      -- will work. The only thing required is a `Tracer.Tracer` in the application
      -- context. If the program execution crosses process boundaries, no serialization
      -- will be performed.
    , MonadTracer
    , Tracer.Tracer(..)
    , Tracer.HasTracer(..)
    , Tracer.runTracer

    -- * Tracing functions
    -- | Functions to trace application code
    , traced
    , traced_
    , startSpan
    , finishSpan

    -- * Propagation
    -- | Functions for serialization and deserialization in a distributed tracing
    -- environment
    , extract
    , inject

    -- * Additional modules
    , module OpenTracing.Log
    , module OpenTracing.Propagation
    , module OpenTracing.Sampling
    , module OpenTracing.Span
    , module OpenTracing.Tags
    , module OpenTracing.Types
    )
where

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           OpenTracing.Log
import           OpenTracing.Propagation hiding (inject, extract)
import qualified OpenTracing.Propagation as Propagation
import           OpenTracing.Sampling
import           OpenTracing.Span
import           OpenTracing.Tags
import qualified OpenTracing.Tracer      as Tracer
import           OpenTracing.Types
import           Prelude                 hiding (span)


type HasOpenTracing   r p   = (Tracer.HasTracer r, HasPropagation r p)
type MonadOpenTracing r p m = (HasOpenTracing r p, MonadReader r m)
type MonadTracer      r   m = (Tracer.HasTracer r, MonadReader r m)
type MonadPropagation r p m = (HasPropagation r p, MonadReader r m)


runOpenTracing :: HasOpenTracing r p => r -> ReaderT r m a -> m a
runOpenTracing = flip runReaderT

-- | Trace a computation as a span. This is a high-level operation that will handle
-- all aspects of the trace, including timing and reporting. If the traced computation
-- throws an excpetion, `traced` will clean up and add logs before rethrowing the
-- exception
--
-- @
--         traced (spanOpts "hello" mempty          ) $ \parent ->
--         traced (spanOpts "world" (childOf parent)) $ \child ->
--            liftIO $ do
--                putStrLn "doing some work..."
--                addLogRecord child (Message "doing some work")
--                threadDelay 500000
-- @
--
traced
    :: ( MonadTracer r m
       , MonadMask     m
       , MonadIO       m
       )
    => SpanOpts
    -- ^ The options to use when creating the span. Options include:
    --
    --   * Operation name
    --
    --   * Tags
    --
    --   * Relations to other spans
    -> (ActiveSpan -> m a)
    -- ^ the computation to trace. The argument is the
    -- span that is created. It can be used to:
    --
    --   * Add logs
    --
    --   * Add child spans
    -> m (Traced  a)
traced opt f = view Tracer.tracer >>= \t -> Tracer.traced t opt f

-- | Variant of `traced` that doesn't return the wrapped value.
traced_
    :: ( MonadTracer r m
       , MonadMask     m
       , MonadIO       m
       )
    => SpanOpts
    -> (ActiveSpan -> m a)
    -> m a
traced_ opt f = tracedResult <$> traced opt f

startSpan
    :: ( MonadTracer r m
       , MonadIO       m
       )
    => SpanOpts
    -> m ActiveSpan
startSpan opt = view Tracer.tracer >>= flip Tracer.startSpan opt

finishSpan
    :: ( MonadTracer r m
       , MonadIO       m
       )
    => ActiveSpan
    -> m FinishedSpan
finishSpan a = view Tracer.tracer >>= flip Tracer.finishSpan a

-- | Serialize a `SpanContext` into the format `c` using a serializer from
-- the application context. See `OpenTracing.Propagation` for more info.
inject
    :: forall c r p m.
       ( MonadPropagation r p m
       , HasCarrier       c p
       )
    => SpanContext
    -> m c
inject ctx = flip Propagation.inject ctx <$> view propagation

-- | Attempt to deserialize a `SpanContext` from the format @c@ using a deserializer
-- from the application context. See `OpenTracing.Propagation` for more info.
extract
    :: forall c r p m.
       ( MonadPropagation r p m
       , HasCarrier       c p
       )
    => c
    -> m (Maybe SpanContext)
extract c = flip Propagation.extract c <$> view propagation
