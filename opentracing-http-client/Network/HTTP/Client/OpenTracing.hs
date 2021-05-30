{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.HTTP.Client.OpenTracing
    ( httpTraced
    , httpTraced'
    )
where

import           Control.Applicative
import           Control.Lens                 (over, set, view)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (decodeUtf8)
import           Network.HTTP.Client.Internal
    ( Manager (mModifyRequest, mModifyResponse)
    , Request (..)
    , getUri
    , responseStatus
    )
import           OpenTracing                  hiding (sampled)
import qualified OpenTracing.Propagation      as Propagation
import qualified OpenTracing.Tracer           as Tracer
import           Prelude                      hiding (span)

-- |
--
-- >>> :{
-- mgr <- newManager defaultManagerSettings
-- rq1 <- parseRequest "http://service1.local/foo"
-- rq2 <- parseRequest "http://service2.local/bar"
-- traced (spanOpts "toplevel" mempty) $ \parent -> do
--     rpc1 <- httpTraced (childOf parent) rq1 mgr httpLbs
--     rpc2 <- httpTraced (childOf parent <> followsFrom (tracedSpan rpc1))
--                        rq2 mgr httpLbs
--     return [tracedResult rpc1, tracedResult rpc2]
-- :}
--
httpTraced
    :: ( HasCarrier       Headers p
       , MonadOpenTracing r       p m
       , MonadIO                    m
       )
    => SpanRefs
    -> Request
    -> Manager
    -> (Request -> Manager -> IO a)
    -> m (Traced a)
httpTraced refs req mgr f = do
    (t,p) <- liftA2 (,) (view tracer) (view propagation)
    liftIO $ httpTraced' t p refs req mgr f

httpTraced'
    :: HasCarrier Headers p
    => Tracer
    -> Propagation        p
    -> SpanRefs
    -> Request
    -> Manager
    -> (Request -> Manager -> IO a)
    -> IO (Traced a)
httpTraced' t p refs req mgr f = do
    sampled <- fmap (view ctxSampled . refCtx) . findParent <$> freezeRefs refs

    let opt = set spanOptSampled sampled
            . set spanOptTags
                  [ HttpMethod  (method req)
                  , HttpUrl     (Text.pack . show $ getUri req)
                  , PeerAddress (decodeUtf8 (host req))
                  , SpanKind    RPCClient
                  ]
            $ spanOpts (decodeUtf8 (path req)) refs

    Tracer.traced t opt $ \span ->
        let mgr' = modMgr span
         in f req { requestManagerOverride = Just mgr' } mgr'
  where
    modMgr span = mgr
        { mModifyRequest  = \rq ->
            inj rq . view spanContext <$> readActiveSpan span

        , mModifyResponse = \rs -> do
            modifyActiveSpan span $
                over spanTags (setTag (HttpStatusCode (responseStatus rs)))
            return rs
        }

    inj rq ctx = rq
        { requestHeaders = requestHeaders rq <> Propagation.inject p ctx
        }
