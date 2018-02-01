{-# LANGUAGE BangPatterns          #-}
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

    , traced
    , traced'

    , traced_
    , traced'_
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

traced'
    :: ( MonadMask  m
       , MonadIO    m
       , HasTracing t
       )
    => t
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m (Traced a)
traced' t opt f = do
    let Tracing{traceStart} = view tracing t
    span <- traceStart opt >>= liftIO . mkActive
    ret  <- withException (f span) (onErr span)
    fin  <- report span
    return Traced { tracedResult = ret, tracedSpan = fin }
  where
    -- /Note/: as per 'withException', we will be reporting any exception incl.
    -- async ones. Exceptions thrown by 'report' will be ignored, and the one
    -- from 'f' will be rethrown. Observe that 'withException' does _not_ run
    -- the error handler under `uninterruptibleMask', unlike 'bracket' -- this
    -- is a good thing, as we might be doing blocking I/O.
    onErr :: MonadIO m => ActiveSpan -> SomeException -> m ()
    onErr span e = do
        liftIO $ do
            now <- getCurrentTime
            modifyActiveSpan span $
                  over spanTags (setTag (Error True))
                . over spanLogs (LogRecord now (ErrObj e :| []) :)
        void $ report span

    report :: MonadIO m => ActiveSpan -> m FinishedSpan
    report a = do
        let Tracing{traceReport} = view tracing t
        span <- liftIO (readActiveSpan a) >>= traceFinish
        case view sampled span of
            Sampled    -> traceReport span
            NotSampled -> return () -- TODO: record metric
        return span

traced'_
    :: ( MonadMask  m
       , MonadIO    m
       , HasTracing t
       )
    => t
    -> SpanOpts
    -> (ActiveSpan -> m a)
    -> m a
traced'_ t opt f = tracedResult <$> traced' t opt f
