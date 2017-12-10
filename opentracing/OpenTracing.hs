{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeApplications       #-}

module OpenTracing
    ( module OpenTracing.Log
    , module OpenTracing.Propagation
    , module OpenTracing.Sampling
    , module OpenTracing.Span
    , module OpenTracing.Tags
    , module OpenTracing.Types

    , Tracing(..)
    , HasTracing(..)
    , runTracing

    , traceInject
    , traceExtract

    , traced
    , traced'

    , traced_
    , traced__
    , traced'_
    , traced'__
    )
where

import Control.Exception.Safe
import Control.Lens
import Control.Monad           (void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Proxy
import Data.Time.Clock
import OpenTracing.Log
import OpenTracing.Propagation
import OpenTracing.Sampling
import OpenTracing.Span
import OpenTracing.Tags
import OpenTracing.Types
import Prelude                 hiding (span)


data Tracing p = Tracing
    { traceStart         :: forall m. MonadIO m => SpanOpts     -> m Span
    , traceReport        :: forall m. MonadIO m => FinishedSpan -> m ()
    , tracingPropagation :: Propagation p
    }


traceInject :: forall c p. HasCarrier c p => Tracing p -> SpanContext -> c
traceInject p = review (carrier (Proxy @c) (tracingPropagation p))

traceExtract :: forall c p. HasCarrier c p => Tracing p -> c -> Maybe SpanContext
traceExtract p = preview (carrier (Proxy @c) (tracingPropagation p))


class HasTracing a p | a -> p where
    tracing :: Lens' a (Tracing p)

instance HasTracing (Tracing p) p where
    tracing = id

runTracing :: Monad m => Tracing p -> ReaderT (Tracing p) m a -> m a
runTracing = flip runReaderT


traced
    :: ( HasTracing   r   p
       , MonadReader  r m
       , MonadMask      m
       , MonadIO        m
       )
    => SpanOpts
    -> (ActiveSpan -> m a)
    -> m (Traced  a)
traced opt f = view tracing >>= \t -> traced' t opt f

traced_
    :: ( HasTracing  r   p
       , MonadReader r m
       , MonadMask     m
       , MonadIO       m
       )
    => SpanOpts
    -> (ActiveSpan -> m a)
    -> m a
traced_ opt f = tracedResult <$> traced opt f

traced__
    :: ( HasTracing  r   p
       , MonadReader r m
       , MonadMask     m
       , MonadIO       m
       )
    => SpanOpts
    -> (ActiveSpan -> m a)
    -> m ()
traced__ opt = void . traced opt

traced'
    :: ( MonadMask m
       , MonadIO   m
       )
    => Tracing p
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m (Traced a)
traced' Tracing{traceStart,traceReport} opt f = mask $ \unmasked -> do
    span <- traceStart opt >>= liftIO . mkActive
    ret  <- unmasked (f span) `catchAny` \e -> do
                liftIO $ do
                    now <- getCurrentTime
                    modifyActiveSpan span $
                          over spanTags (setTag (Error True))
                        . over spanLogs (LogRecord now (ErrObj e :| []) :)
                !_ <- report span
                throwM e
    fin  <- report span
    return Traced { tracedResult = ret, tracedSpan = fin }
  where
    report a = do
        span <- liftIO (readActiveSpan a) >>= traceFinish
        case view sampled span of
            Sampled    -> traceReport span
            NotSampled -> return () -- TODO: record metric
        return span

traced'_
    :: ( MonadMask m
       , MonadIO   m
       )
    => Tracing p
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m a
traced'_ t opt f = tracedResult <$> traced' t opt f

traced'__
    :: ( MonadMask m
       , MonadIO   m
       )
    => Tracing p
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m ()
traced'__ t opt = void . traced' t opt
