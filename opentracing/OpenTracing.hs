{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module OpenTracing
    ( module OpenTracing.Propagation
    , module OpenTracing.Sampling
    , module OpenTracing.Span
    , module OpenTracing.Tags
    , module OpenTracing.Types

    , Tracing(..)

    , traced
    , traced'

    , traced_
    , traced__
    , traced'_
    , traced'__

    , runTracing
    )
where

import Control.Lens
import Control.Monad           (void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader    (MonadReader, ask)
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Time.Clock
import OpenTracing.Class
import OpenTracing.Log
import OpenTracing.Propagation
import OpenTracing.Sampling
import OpenTracing.Span
import OpenTracing.Tags
import OpenTracing.Types
import Prelude                 hiding (span)


traced
    :: ( HasSampled  ctx
       , MonadReader (Tracing ctx MonadIO) m
       , MonadMask   m
       , MonadIO     m
       )
    => SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m (Traced ctx a)
traced opt f = ask >>= \t -> traced' t opt f

traced_
    :: ( HasSampled  ctx
       , MonadReader (Tracing ctx MonadIO) m
       , MonadMask   m
       , MonadIO     m
       )
    => SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m a
traced_ opt f = tracedResult <$> traced opt f

traced__
    :: ( HasSampled  ctx
       , MonadReader (Tracing ctx MonadIO) m
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
    => Tracing     ctx MonadIO
    -> SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m (Traced ctx a)
traced' t@Tracing{runTrace} opt f = mask $ \unmasked -> do
    span <- interpret runTrace (traceStart opt) >>= liftIO . mkActive
    ret  <- unmasked (f span) `catchAll` \e -> liftIO $ do
                now <- getCurrentTime
                modifyActiveSpan span $
                      over spanTags (setTag (Error True))
                    . over spanLogs (LogRecord now (ErrObj e :| []) :)
                _   <- report t span
                throwM e
    fin  <- report t span
    return Traced { tracedResult = ret, tracedSpan = fin }


traced'_
    :: ( HasSampled ctx
       , MonadMask  m
       , MonadIO    m
       )
    => Tracing     ctx MonadIO
    -> SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m a
traced'_ t opt f = tracedResult <$> traced' t opt f

traced'__
    :: ( HasSampled ctx
       , MonadMask  m
       , MonadIO    m
       )
    => Tracing     ctx MonadIO
    -> SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m ()
traced'__ t opt = void . traced' t opt

report
    :: ( HasSampled ctx
       , MonadIO    m
       )
    => Tracing    ctx MonadIO
    -> ActiveSpan ctx
    -> m (FinishedSpan ctx)
report Tracing{runTrace,runReport} a = do
    span <- liftIO (readActiveSpan a) >>= \s ->
                interpret runTrace $ traceFinish s
    case view ctxSampled span of
        Sampled    -> interpret runReport $ traceReport span
        NotSampled -> return () -- TODO: record metric
    return span
