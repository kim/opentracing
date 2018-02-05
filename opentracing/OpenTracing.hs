{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE StrictData       #-}

module OpenTracing
    ( module OpenTracing.Log
    , module OpenTracing.Propagation
    , module OpenTracing.Sampling
    , module OpenTracing.Span
    , module OpenTracing.Tags
    , module OpenTracing.Types

    , HasOpenTracing
    , MonadOpenTracing
    , runOpenTracing

    , MonadTracer
    , Tracer.Tracer(..)
    , Tracer.HasTracer(..)
    , Tracer.runTracer

    , traced
    , traced_
    , startSpan
    , finishSpan
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

traced
    :: ( MonadTracer r m
       , MonadMask     m
       , MonadIO       m
       )
    => SpanOpts
    -> (ActiveSpan -> m a)
    -> m (Traced  a)
traced opt f = view Tracer.tracer >>= \t -> Tracer.traced t opt f

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

inject
    :: forall c r p m.
       ( MonadPropagation r p m
       , HasCarrier       c p
       )
    => SpanContext
    -> m c
inject ctx = flip Propagation.inject ctx <$> view propagation

extract
    :: forall c r p m.
       ( MonadPropagation r p m
       , HasCarrier      c p
       )
    => c
    -> m (Maybe SpanContext)
extract c = flip Propagation.extract c <$> view propagation
