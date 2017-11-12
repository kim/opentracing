{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}

module OpenTracing
    ( module OpenTracing.Propagation
    , module OpenTracing.Sampling
    , module OpenTracing.Span
    , module OpenTracing.Tags
    , module OpenTracing.Types

    , Tracing(..)

    , traced
    , runTracing

    , traced'

    , traced_
    , traced__
    , traced'_
    , traced'__
    )
where

import Control.Lens
import Control.Monad           (void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Time.Clock
import OpenTracing.Log
import OpenTracing.Propagation
import OpenTracing.Sampling
import OpenTracing.Span
import OpenTracing.Tags
import OpenTracing.Types
import Prelude                 hiding (span)


data Tracing ctx = Tracing
    { traceStart  :: forall m. MonadIO m => SpanOpts     ctx -> m (Span ctx)
    , traceReport :: forall m. MonadIO m => FinishedSpan ctx -> m ()
    }

runTracing :: Monad m => Tracing ctx -> ReaderT (Tracing ctx) m a -> m a
runTracing = flip runReaderT

traced
    :: ( HasSampled  ctx
       , MonadReader (Tracing ctx) m
       , MonadMask   m
       , MonadIO     m
       )
    => SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m (Traced   ctx a)
traced opt f = ask >>= \t -> traced' t opt f

traced_
    :: ( HasSampled  ctx
       , MonadReader (Tracing ctx) m
       , MonadMask   m
       , MonadIO     m
       )
    => SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m a
traced_ opt f = tracedResult <$> traced opt f

traced__
    :: ( HasSampled  ctx
       , MonadReader (Tracing ctx) m
       , MonadMask   m
       , MonadIO     m
       )
    => SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m ()
traced__ opt = void . traced opt

traced'
    :: ( HasSampled ctx
       , MonadMask  m
       , MonadIO    m
       )
    => Tracing     ctx
    -> SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m (Traced   ctx a)
traced' t@Tracing{traceStart,traceReport} opt f = mask $ \unmasked -> do
    span <- traceStart opt >>= liftIO . mkActive
    ret  <- unmasked (f span) `catchAll` \e -> do
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
        case view ctxSampled span of
            Sampled    -> traceReport span
            NotSampled -> return () -- TODO: record metric
        return span

traced'_
    :: ( HasSampled ctx
       , MonadMask  m
       , MonadIO    m
       )
    => Tracing     ctx
    -> SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m a
traced'_ t opt f = tracedResult <$> traced' t opt f

traced'__
    :: ( HasSampled ctx
       , MonadMask  m
       , MonadIO    m
       )
    => Tracing     ctx
    -> SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m ()
traced'__ t opt = void . traced' t opt
