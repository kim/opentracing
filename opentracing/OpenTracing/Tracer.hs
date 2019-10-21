{-|
Module: OpenTracing.Tracer

This module provides mid and high level tracing functions.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE StrictData     #-}

module OpenTracing.Tracer
    ( Tracer(..)
    , HasTracer(..)
    , runTracer

    , traced
    , traced_
    , startSpan
    , finishSpan
    )
where

import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List.NonEmpty     (NonEmpty (..))
import Data.Time.Clock        (getCurrentTime)
import OpenTracing.Log
import OpenTracing.Span
import OpenTracing.Tags
import Prelude                hiding (span)

-- | A `Tracer` is a set of effectful actions that define the mid-level interface
-- to an [OpenTracing tracer](https://github.com/opentracing/specification/blob/master/specification.md#tracer)
--
-- Appliction code should generally construct a `Tracer` once and then use other
-- higher-level functions such as `traced`, `startSpan`, `finishedSpan`.
--
-- @since 0.1.0.0
data Tracer = Tracer
    { tracerStart  :: forall m. MonadIO m => SpanOpts     -> m Span
      -- ^ Start recording a new span with the given options. This is
      -- a mid-level operation that will handle start timing and random span ID
      -- generation.
      --
      -- Application code should supply this field with `stdTracer`.
    , tracerReport :: forall m. MonadIO m => FinishedSpan -> m ()
    -- ^ Report a finished span. What reporting means for each application will
    -- depend on where this data is going. There are multiple backends that define
    -- reporters for Google Cloudtrace, Zipkin, and Jaeger, for example.
    }

-- | Typeclass for application environments that contain a `Tracer`.
--
-- @since 0.1.0.0
class HasTracer a where
    tracer :: Getting r a Tracer

instance HasTracer Tracer where
    tracer = id

runTracer :: HasTracer r => r -> ReaderT r m a -> m a
runTracer = flip runReaderT

-- | Trace a computation as a span. This is a high-level operation that will handle
-- all aspects of the trace, including timing and reporting. If the traced computation
-- throws an excpetion, `traced` will clean up and add logs before rethrowing the
-- exception
--
-- @
--         traced_ (spanOpts "hello" mempty          ) $ \parent ->
--         traced_ (spanOpts "world" (childOf parent)) $ \child ->
--            liftIO $ do
--                putStrLn "doing some work..."
--                addLogRecord child (Message "doing some work")
--                threadDelay 500000
-- @
--
-- @since 0.1.0.0
traced
    :: ( HasTracer t
       , MonadMask m
       , MonadIO   m
       )
    => t -- ^ A tracer environemtn
    -> SpanOpts -- ^ The options to use when creating the span. Options include:
    --
    --   * Operation name
    --
    --   * Tags
    --
    --   * Relations to other spans
    -> (ActiveSpan -> m a) -- ^ the computation to trace. The argument is the
    -- span that is created. It can be used to:
    --
    --   * Add logs
    --
    --   * Add child spans
    -> m (Traced a)
traced t opt f = do
    span <- startSpan t opt
    -- /Note/: as per 'withException', we will be reporting any exception incl.
    -- async ones. Exceptions thrown by 'finishSpan'' will be ignored, and the
    -- one from 'f' will be rethrown. Observe that 'withException' does _not_
    -- run the error handler under `uninterruptibleMask', unlike 'bracket'. This
    -- is a good thing, as we might be doing blocking I/O.
    ret  <- withException (f span) (onErr span >=> void . finishSpan t)
    fin  <- finishSpan t span
    return Traced { tracedResult = ret, tracedSpan = fin }
  where
    onErr :: MonadIO m => ActiveSpan -> SomeException -> m ActiveSpan
    onErr span e = liftIO $ do
        now <- getCurrentTime
        modifyActiveSpan span $
              over spanTags (setTag (Error True))
            . over spanLogs (LogRecord now (ErrObj e :| []) :)
        pure span

-- | Variant of `traced` that doesn't return the wrapped value.
--
-- @since 0.1.0.0
traced_
    :: ( HasTracer t
       , MonadMask m
       , MonadIO   m
       )
    => t
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m a
traced_ t opt f = tracedResult <$> traced t opt f

-- | Start recording a span
--
-- @since 0.1.0.0
startSpan :: (HasTracer t, MonadIO m) => t -> SpanOpts -> m ActiveSpan
startSpan t opt = do
    let Tracer{tracerStart} = view tracer t
    tracerStart opt >>= liftIO . mkActive

-- | Finish recording a span
--
-- @since 0.1.0.0
finishSpan :: (HasTracer t, MonadIO m) => t -> ActiveSpan -> m FinishedSpan
finishSpan t a = do
    let Tracer{tracerReport} = view tracer t
    span <- liftIO (readActiveSpan a) >>= spanFinish
    case view sampled span of
        Sampled    -> tracerReport span
        NotSampled -> return () -- TODO: record metric
    return span
