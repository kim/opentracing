{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module OpenTracing.Class where

import           Control.Category
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader
import           OpenTracing.Types
import           Prelude                hiding (id, (.))
import qualified Prelude


newtype Interpret c d = Interpret
    { interpret :: forall n a. d n => (forall m. c m => m a) -> n a
    }

instance Category Interpret where
    id = Interpret Prelude.id
    Interpret f . Interpret g = Interpret $ \h -> f (g h)

class Monad m => MonadTrace ctx m where
    traceStart  :: SpanOpts ctx -> m (Span ctx)
    traceFinish :: Span     ctx -> m (FinishedSpan ctx)

    default traceFinish :: MonadIO m => Span ctx -> m (FinishedSpan ctx)
    traceFinish = defaultTraceFinish

class Monad m => MonadReport ctx m where
    traceReport :: FinishedSpan ctx -> m ()

data Tracing ctx eff = Tracing
    { runTrace  :: Interpret (MonadTrace  ctx) eff
    , runReport :: Interpret (MonadReport ctx) eff
    }

runTracing :: Monad m => Tracing ctx eff -> ReaderT (Tracing ctx eff) m a -> m a
runTracing = flip runReaderT
