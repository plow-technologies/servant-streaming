{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Streaming.Server.Internal where

import           Control.Exception                          (bracket)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource               (ResourceT,
                                                             runResourceT,
                                                             InternalState,
                                                             createInternalState,
                                                             closeInternalState,
                                                             runInternalState)
import qualified Data.ByteString                            as BS
import           Data.Maybe                                 (fromMaybe)
import           GHC.TypeLits                               (KnownNat, natVal)
import qualified Network.HTTP.Media                         as M
import           Network.HTTP.Types                         (Method, Status,
                                                             hAccept,
                                                             hContentType)
import           Network.Wai                                (Response,
                                                             requestHeaders)
import           Network.Wai.Streaming                      (streamingRequest,
                                                             streamingResponse)
import           Servant                                    hiding (Stream)
import           Servant.API.ContentTypes                   (AllMime (allMime))
import           Servant.Server.Internal                    (ct_wildcard,
                                                             methodCheck,
                                                             acceptCheck)
import           Servant.Server.Internal.Router             (leafRouter)
import           Servant.Server.Internal.RoutingApplication (DelayedIO,
                                                             RouteResult (Route),
                                                             addBodyCheck,
                                                             addMethodCheck,
                                                             addAcceptCheck,
                                                             delayedFailFatal,
                                                             runAction,
                                                             withRequest)
import           Servant.Streaming
import           Streaming

instance ( AllMime contentTypes, HasServer subapi ctx
         ) => HasServer (StreamBody contentTypes :> subapi) ctx where
  type ServerT (StreamBody contentTypes :> subapi) m
    = (M.MediaType, Stream (Of BS.ByteString) (ResourceT IO) ())
      -> ServerT subapi m

  route _ ctxt subapi =
    route (Proxy :: Proxy subapi) ctxt
      $ addBodyCheck subapi getContentType makeBody
      where
        getContentType :: DelayedIO M.MediaType
        getContentType = withRequest $ \request -> do
          let contentTypeHdr
               = fromMaybe ("application" M.// "octet-stream")
               $ lookup hContentType (requestHeaders request) >>= M.parseAccept
          if contentTypeHdr `elem` contentTypeList
            then return contentTypeHdr else delayedFailFatal err415

        contentTypeList :: [M.MediaType]
        contentTypeList = allMime (Proxy :: Proxy contentTypes)

        makeBody :: MonadIO m => a -> DelayedIO (a, Stream (Of BS.ByteString) m ())
        makeBody a = withRequest $ \req -> return (a, streamingRequest req)

  hoistServerWithContext _ a b c
    = hoistServerWithContext (Proxy :: Proxy subapi) a b . c


instance ( KnownNat status, AllMime contentTypes, ReflectMethod method
         ) => HasServer (StreamResponse method status contentTypes) ctx where
  type ServerT (StreamResponse method status contentTypes) m
    = m (Stream (Of BS.ByteString) (ResourceT IO) ())

  route _ _ctxt subapi = leafRouter $ \env request respond ->
    let action = subapi `addMethodCheck` methodCheck method request
                        `addAcceptCheck` acceptCheck contentTypeProxy accept
        accept = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
    in bracket createInternalState
               closeInternalState
               (runAction action env request respond . streamResponse)
    where
      method :: Method
      method = reflectMethod (Proxy :: Proxy method)

      contentTypeProxy :: Proxy contentTypes
      contentTypeProxy = Proxy

      streamResponse :: InternalState -> Stream (Of BS.ByteString) (ResourceT IO) () -> RouteResult Response
      streamResponse st stream = Route $ streamingResponse (hoist (`runInternalState` st) stream) status []

      status :: Status
      status = toEnum $ fromInteger $ natVal (Proxy :: Proxy status)

  hoistServerWithContext _ _ b c
    = b c
