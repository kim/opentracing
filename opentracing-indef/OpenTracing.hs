module OpenTracing
    ( module OpenTracing.Types

    , Context
    , Tracer
    , Env
    , MonadTracer(..)

    , newEnv
    , runTracer

    , traceStart
    , traceStart'
    , traceFinish
    )
where

import           Control.Monad.Catch
import           Control.Monad.IO.Class    (MonadIO(..))
import           Data.HashSet              (HashSet)
import           Data.Set                  (Set)
import           Data.Text                 (Text)
import qualified OpenTracing.Reporter      as OT
import           OpenTracing.Tracer        (Context, Env, Tracer, newEnv, runTracer)
import qualified OpenTracing.Tracer        as OT
import           OpenTracing.Tracer.Class  (MonadTracer (..))
import           OpenTracing.Types         hiding (traceFinish)
import qualified OpenTracing.Types


traceStart'
    :: ( MonadTracer m
       , MonadMask   m
       , MonadIO     m
       )
    => Text
    -> HashSet (Reference Context)
    -> Set Tag
    -> (Span Context -> m a)
    -> m a
traceStart' n refs tags = bracket (traceStart n refs tags) traceFinish

traceStart
    :: ( MonadTracer m
       , MonadIO     m
       )
    => Text
    -> HashSet (Reference Context)
    -> Set Tag
    -> m (Span Context)
traceStart n refs tags = liftTracer $ OT.traceStart n refs tags

traceFinish :: (MonadTracer m, MonadIO m) => Span Context -> m ()
traceFinish s = OpenTracing.Types.traceFinish s >>= liftTracer . OT.traceReport
