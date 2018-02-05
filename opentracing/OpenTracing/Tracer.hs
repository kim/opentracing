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


data Tracer = Tracer
    { tracerStart  :: forall m. MonadIO m => SpanOpts     -> m Span
    , tracerReport :: forall m. MonadIO m => FinishedSpan -> m ()
    }

class HasTracer a where
    tracer :: Getting r a Tracer

instance HasTracer Tracer where
    tracer = id

runTracer :: HasTracer r => r -> ReaderT r m a -> m a
runTracer = flip runReaderT

traced
    :: ( HasTracer t
       , MonadMask m
       , MonadIO   m
       )
    => t
    -> SpanOpts
    -> (ActiveSpan -> m a)
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

startSpan :: (HasTracer t, MonadIO m) => t -> SpanOpts -> m ActiveSpan
startSpan t opt = do
    let Tracer{tracerStart} = view tracer t
    tracerStart opt >>= liftIO . mkActive

finishSpan :: (HasTracer t, MonadIO m) => t -> ActiveSpan -> m FinishedSpan
finishSpan t a = do
    let Tracer{tracerReport} = view tracer t
    span <- liftIO (readActiveSpan a) >>= spanFinish
    case view sampled span of
        Sampled    -> tracerReport span
        NotSampled -> return () -- TODO: record metric
    return span
