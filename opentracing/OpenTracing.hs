{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}

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

    -- * 'MonadReader' interface
    , traced
    , traced_
    , startSpan
    , finishSpan

    -- * Interface requiring explicit threading of 'Tracing'
    , traced'
    , traced'_
    , startSpan'
    , finishSpan'
    )
where

import Control.Exception.Safe
import Control.Lens
import Control.Monad           (void)
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


data Tracing = Tracing
    { traceStart  :: forall m. MonadIO m => SpanOpts     -> m Span
    , traceReport :: forall m. MonadIO m => FinishedSpan -> m ()
    }

class HasTracing a where
    tracing :: Getting r a Tracing

instance HasTracing Tracing where
    tracing = id

runTracing :: Monad m => Tracing -> ReaderT Tracing m a -> m a
runTracing = flip runReaderT


traced
    :: ( HasTracing  r
       , MonadReader r m
       , MonadMask     m
       , MonadIO       m
       )
    => SpanOpts
    -> (ActiveSpan -> m a)
    -> m (Traced  a)
traced opt f = view tracing >>= \t -> traced' t opt f

traced_
    :: ( HasTracing  r
       , MonadReader r m
       , MonadMask     m
       , MonadIO       m
       )
    => SpanOpts
    -> (ActiveSpan -> m a)
    -> m a
traced_ opt f = tracedResult <$> traced opt f

startSpan
    :: ( HasTracing  t
       , MonadReader t m
       , MonadIO       m
       )
    => SpanOpts
    -> m ActiveSpan
startSpan opt = view tracing >>= \t -> startSpan' t opt

finishSpan
    :: ( HasTracing  t
       , MonadReader t m
       , MonadIO       m
       )
    => ActiveSpan
    -> m FinishedSpan
finishSpan a = view tracing >>= \t -> finishSpan' t a


traced'
    :: ( HasTracing t
       , MonadMask  m
       , MonadIO    m
       )
    => t
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m (Traced a)
traced' t opt f = do
    span <- startSpan' t opt
    -- /Note/: as per 'withException', we will be reporting any exception incl.
    -- async ones. Exceptions thrown by 'finishSpan'' will be ignored, and the
    -- one from 'f' will be rethrown. Observe that 'withException' does _not_
    -- run the error handler under `uninterruptibleMask', unlike 'bracket'. This
    -- is a good thing, as we might be doing blocking I/O.
    ret  <- withException (f span) (onErr span >=> void . finishSpan' t)
    fin  <- finishSpan' t span
    return Traced { tracedResult = ret, tracedSpan = fin }
  where
    onErr :: MonadIO m => ActiveSpan -> SomeException -> m ActiveSpan
    onErr span e = liftIO $ do
        now <- getCurrentTime
        modifyActiveSpan span $
              over spanTags (setTag (Error True))
            . over spanLogs (LogRecord now (ErrObj e :| []) :)
        pure span

traced'_
    :: ( HasTracing t
       , MonadMask  m
       , MonadIO    m
       )
    => t
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m a
traced'_ t opt f = tracedResult <$> traced' t opt f

startSpan' :: (HasTracing t, MonadIO m) => t -> SpanOpts -> m ActiveSpan
startSpan' t opt = do
    let Tracing{traceStart} = view tracing t
    traceStart opt >>= liftIO . mkActive

finishSpan' :: (HasTracing t, MonadIO m) => t -> ActiveSpan -> m FinishedSpan
finishSpan' t a = do
    let Tracing{traceReport} = view tracing t
    span <- liftIO (readActiveSpan a) >>= traceFinish
    case view sampled span of
        Sampled    -> traceReport span
        NotSampled -> return () -- TODO: record metric
    return span
