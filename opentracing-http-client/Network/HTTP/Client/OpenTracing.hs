{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.HTTP.Client.OpenTracing where

import           Control.Lens                 (over, view)
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Semigroup               ((<>))
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (decodeUtf8)
import           Network.HTTP.Client.Internal
    ( Manager (mModifyRequest, mModifyResponse)
    , Request (..)
    , getUri
    , responseStatus
    )
import           OpenTracing
import           Prelude                      hiding (span)

-- |
--
-- >>> httpTraced tracing [ChildOf parent] req mgr httpLbs
--
-- >>> httpTraced tracing [ChildOf parent] req mgr $ \r m -> withResponse r m brConsume
--
httpTraced
    :: ( HasSampled ctx
       , AsCarrier  HttpHeaders ctx ctx
       )
    => Tracing ctx MonadIO
    -> [Reference ctx]
    -> Request
    -> Manager
    -> (Request -> Manager -> IO a)
    -> IO a
httpTraced tracing refs req mgr f = do
    let opt = SpanOpts
            { spanOptOperation = decodeUtf8 $ path req
            , spanOptRefs      = refs
            , spanOptTags      =
                [ HttpMethod  (method req)
                , HttpUrl     (Text.pack . show $ getUri req)
                , PeerAddress (decodeUtf8 (host req))
                , SpanKind    RPCClient
                ]
            , spanOptSampled   = Nothing -- XXX
            }

    traced' tracing opt $ \span ->
        let mgr' = modMgr span
         in f (req { requestManagerOverride = Just mgr' }) mgr'
  where
    modMgr span = mgr
        { mModifyRequest  = pure . inject (view spanContext span)
        , mModifyResponse = \res -> do
            modifyActiveSpan span $
                over spanTags (setTag (HttpStatusCode (responseStatus res)))
            return res
        }

    inject ctx rq = rq
        { requestHeaders = requestHeaders rq <> fromHttpHeaders (traceInject ctx)
        }
