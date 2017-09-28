{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module OpenTracing
    ( module OpenTracing.Sampling
    , module OpenTracing.Types
    , Tracing(..)
    , traced
    , runTracing
    )
where

import Control.Lens           (view)
import Control.Monad.Catch    (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader, ask)
import OpenTracing.Class
import OpenTracing.Sampling
import OpenTracing.Types


traced
    :: ( HasSampled  ctx
       , MonadReader (Tracing ctx MonadIO) m
       , MonadMask   m
       , MonadIO     m
       )
    => SpanOpts ctx
    -> (Span ctx -> m a)
    -> m a
traced opt f = do
    Tracing{..} <- ask
    bracket (interpret runTrace $ traceStart opt)
            (\s -> case view ctxSampled s of
                Sampled -> do
                    fs <- interpret runTrace $ traceFinish s
                    interpret runReport $ traceReport fs
                NotSampled -> return () -- TODO: record metric
            )
            f
