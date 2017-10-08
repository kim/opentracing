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

    , runTracing
    )
where

import Control.Lens
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
    -> m a
traced opt f = ask >>= \t -> traced' t opt f

traced'
    :: ( HasSampled ctx
       , MonadMask  m
       , MonadIO    m
       )
    => Tracing     ctx MonadIO
    -> SpanOpts    ctx
    -> (ActiveSpan ctx -> m a)
    -> m a
traced' t@Tracing{runTrace} opt f = mask $ \unmasked -> do
    span <- interpret runTrace (traceStart opt) >>= liftIO . mkActive
    ret  <- unmasked (f span) `catchAll` \e -> liftIO $ do
                now <- getCurrentTime
                modifyActiveSpan span $
                      over spanTags (setTag (Error True))
                    . over spanLogs (LogRecord now (ErrObj e :| []) :)
                report t span
                throwM e
    report t span
    return ret

report
    :: ( HasSampled ctx
       , MonadIO    m
       )
    => Tracing    ctx MonadIO
    -> ActiveSpan ctx
    -> m ()
report Tracing{runTrace,runReport} a = do
    span <- liftIO $ readActiveSpan a
    case view ctxSampled span of
        Sampled -> do
            finished <- interpret runTrace $ traceFinish span
            interpret runReport $ traceReport finished
        NotSampled -> return () -- TODO: record metric
