module OpenTracing.Reporter.Config.Simple where

import Control.Monad.IO.Class (MonadIO)
import OpenTracing.Types


type Config = ()

loadConfig :: MonadIO m => ConfigSource -> m Config
loadConfig _ = return ()
