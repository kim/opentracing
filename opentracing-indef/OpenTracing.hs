module OpenTracing
    ( module OpenTracing.Reporter
    , module OpenTracing.Reporter.Config
    , module OpenTracing.Tracer
    , module OpenTracing.Types

    , traceStartSafe
    , traceFinish
    )
where

import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO)
import           Data.HashSet                (HashSet)
import           Data.Set                    (Set)
import           Data.Text                   (Text)
import           OpenTracing.Reporter
import           OpenTracing.Reporter.Config
import           OpenTracing.Tracer
import           OpenTracing.Types           hiding (traceFinish)
import qualified OpenTracing.Types


traceStartSafe
    :: (MonadMask m, MonadIO m)
    => Text
    -> HashSet (Reference Context)
    -> Set Tag
    -> (Span Context -> Tracer m a)
    -> Tracer m a
traceStartSafe n refs tags = bracket (traceStart n refs tags) traceFinish

traceFinish :: MonadIO m => Span Context -> Tracer m ()
traceFinish s = OpenTracing.Types.traceFinish s >>= traceReport
