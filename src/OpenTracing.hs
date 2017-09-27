{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module OpenTracing
    ( module OpenTracing.Types
    , Tracing(..)
    , traced
    , runTracing
    )
where

import Control.Monad.Catch    (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader, ask)
import OpenTracing.Class
import OpenTracing.Types


traced
    :: ( MonadReader (Tracing ctx MonadIO) m
       , MonadMask   m
       , MonadIO     m
       )
    => SpanOpts ctx
    -> (Span ctx -> m a)
    -> m a
traced opt f = do
    Tracing{..} <- ask
    bracket (interpret runTrace $ traceStart opt)
            (\s -> do
                fs <- interpret runTrace $ traceFinish s
                interpret runReport $ traceReport fs
            )
            f
