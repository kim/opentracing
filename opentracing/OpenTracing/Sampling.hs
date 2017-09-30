module OpenTracing.Sampling where

import Data.Text (Text)


type Sampler t m = t -> Text -> m Bool

constSampler :: Applicative m => Bool -> Sampler t m
constSampler x _ _ = pure x
