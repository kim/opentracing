{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

module OpenTracing.Span
    ( Span
    , newSpan

    , ActiveSpan
    , mkActive
    , modifyActiveSpan
    , readActiveSpan

    , FinishedSpan
    , defaultTraceFinish

    , spanContext
    , spanOperation
    , spanStart
    , spanTags
    , spanRefs
    , spanLogs
    , spanDuration

    , SpanOpts(..)
    , spanOpts

    , Reference(..)

    , Sampled(..)
    , HasSampled(..)
    )
where

import           Control.Lens           hiding (op)
import           Control.Monad.IO.Class
import           Data.Aeson             (ToJSON (..))
import           Data.Aeson.Encoding
import           Data.Foldable
import           Data.Hashable
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as HashSet
import           Data.IORef
import           Data.Text              (Text)
import           Data.Time.Clock
import           GHC.Generics           (Generic)
import           OpenTracing.Log
import           OpenTracing.Tags
import           Prelude                hiding (span)


data Sampled = NotSampled | Sampled
    deriving (Eq, Show, Read, Bounded, Enum, Generic)

instance Hashable Sampled

instance ToJSON Sampled where
    toJSON     = toJSON . fromEnum
    toEncoding = int . fromEnum

class HasSampled ctx where
    ctxSampled :: Lens' ctx Sampled


data Reference ctx
    = ChildOf     { refCtx :: ctx }
    | FollowsFrom { refCtx :: ctx }
    deriving (Eq, Show, Generic)

instance Hashable ctx => Hashable (Reference ctx)


data SpanOpts ctx = SpanOpts
    { spanOptOperation :: Text
    , spanOptRefs      :: [Reference ctx]
    , spanOptTags      :: [Tag]
    , spanOptSampled   :: Maybe Sampled
    -- ^ Force 'Span' to be sampled (or not).
    -- 'Nothing' denotes leave decision to 'Sampler' (the default)
    }

spanOpts :: Text -> [Reference ctx] -> SpanOpts ctx
spanOpts op ref = SpanOpts
    { spanOptOperation = op
    , spanOptRefs      = ref
    , spanOptTags      = mempty
    , spanOptSampled   = Nothing
    }

data Span ctx = Span
    { _spanContext   :: ctx
    , _spanOperation :: Text
    , _spanStart     :: UTCTime
    , _spanTags      :: Tags
    , _spanRefs      :: HashSet (Reference ctx)
    , _spanLogs      :: [LogRecord]
    } deriving Show

newSpan
    :: ( MonadIO     m
       , Eq          ctx
       , Hashable    ctx
       , Foldable    t
       , Foldable    u
       )
    => ctx
    -> Text
    -> t (Reference ctx)
    -> u Tag
    -> m (Span ctx)
newSpan ctx op rs ts = do
    t <- liftIO getCurrentTime
    pure Span
        { _spanContext   = ctx
        , _spanOperation = op
        , _spanStart     = t
        , _spanTags      = foldMap (`setTag` mempty) ts
        , _spanRefs      = HashSet.fromList . toList $ rs
        , _spanLogs      = mempty
        }


data ActiveSpan ctx = ActiveSpan
    { _activeSpan   :: Span ctx
    , mutActiveSpan :: IORef (Span ctx)
    }

mkActive :: Span ctx -> IO (ActiveSpan ctx)
mkActive s = ActiveSpan s <$> newIORef s

modifyActiveSpan :: ActiveSpan ctx -> (Span ctx -> Span ctx) -> IO ()
modifyActiveSpan ActiveSpan{mutActiveSpan} f
    = atomicModifyIORef' mutActiveSpan ((,()) . f)

readActiveSpan :: ActiveSpan ctx -> IO (Span ctx)
readActiveSpan = readIORef . mutActiveSpan


data FinishedSpan ctx = FinishedSpan
    { _spanSpan     :: Span ctx
    , _spanDuration :: NominalDiffTime
    } deriving Show

defaultTraceFinish :: MonadIO m => Span ctx -> m (FinishedSpan ctx)
defaultTraceFinish s = do
    t <- liftIO getCurrentTime
    pure FinishedSpan
        { _spanSpan     = s
        , _spanDuration = diffUTCTime t (_spanStart s)
        }

makeClassy ''Span
makeLenses ''ActiveSpan
makeLenses ''FinishedSpan

instance HasSpan (FinishedSpan ctx) ctx where
    span = spanSpan

instance HasSpan (ActiveSpan ctx) ctx where
    span = activeSpan

instance HasSampled ctx => HasSampled (Span ctx) where
    ctxSampled = spanContext . ctxSampled

instance HasSampled ctx => HasSampled (FinishedSpan ctx) where
    ctxSampled = spanContext . ctxSampled
