{-# LANGUAGE FlexibleInstances #-}

module OpenTracing.Tracer.Class
    ( MonadTracer(..)
    )
where

import           Control.Monad.IO.Class       (MonadIO(..))
import qualified Control.Monad.RWS.Lazy       as LRW
import qualified Control.Monad.RWS.Strict     as RW
import qualified Control.Monad.State.Lazy     as LS
import qualified Control.Monad.State.Strict   as S
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.List     (ListT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Trans.Reader   (ReaderT)
import qualified Control.Monad.Writer.Lazy    as LW
import qualified Control.Monad.Writer.Strict  as W
import           OpenTracing.Tracer


class (Functor m, Applicative m, Monad m, MonadIO m) => MonadTracer m where
    liftTracer :: Tracer IO a -> m a

instance MonadTracer (Tracer IO) where
    liftTracer = id

instance MonadTracer m => MonadTracer (IdentityT   m) where liftTracer = lift . liftTracer
instance MonadTracer m => MonadTracer (ListT       m) where liftTracer = lift . liftTracer
instance MonadTracer m => MonadTracer (MaybeT      m) where liftTracer = lift . liftTracer
instance MonadTracer m => MonadTracer (ExceptT   e m) where liftTracer = lift . liftTracer
instance MonadTracer m => MonadTracer (ReaderT   r m) where liftTracer = lift . liftTracer
instance MonadTracer m => MonadTracer (S.StateT  s m) where liftTracer = lift . liftTracer
instance MonadTracer m => MonadTracer (LS.StateT s m) where liftTracer = lift . liftTracer

instance (Monoid w, MonadTracer m) => MonadTracer (W.WriterT w m) where
    liftTracer = lift . liftTracer

instance (Monoid w, MonadTracer m) => MonadTracer (LW.WriterT w m) where
    liftTracer = lift . liftTracer

instance (Monoid w, MonadTracer m) => MonadTracer (RW.RWST r w s m) where
    liftTracer = lift . liftTracer

instance (Monoid w, MonadTracer m) => MonadTracer (LRW.RWST r w s m) where
    liftTracer = lift . liftTracer
