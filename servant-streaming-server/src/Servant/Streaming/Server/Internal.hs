{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Streaming.Server.Internal where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource               (ResourceT,
                                                             runResourceT)
import qualified Data.ByteString                            as BS
import           Data.Maybe                                 (fromMaybe)
import           GHC.TypeLits                               (KnownNat, natVal)
import qualified Network.HTTP.Media                         as M
import           Network.HTTP.Types                         (Status,
                                                             hContentType)
import           Network.Wai                                (Request, Response,
                                                             requestBody,
                                                             requestHeaders)
import           Network.Wai.Streaming                      (streamingRequest,
                                                             streamingResponse)
import           Servant
import           Servant.API.ContentTypes                   (AllMime (allMime))
import           Servant.Server.Internal.Router             (leafRouter)
import           Servant.Server.Internal.RoutingApplication (DelayedIO,
                                                             RouteResult (Route),
                                                             addBodyCheck,
                                                             delayedFailFatal,
                                                             runAction,
                                                             withRequest)
import           Servant.Streaming
import           Streaming
import qualified Streaming.Prelude as S

instance ( AllMime contentTypes, HasServer subapi ctx
         ) => HasServer (StreamBody contentTypes :> subapi) ctx where
  type ServerT (StreamBody contentTypes :> subapi) m
    = (M.MediaType, Stream (Of BS.ByteString) (ResourceT IO) ()) -> ServerT subapi m

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


{-streamingReq :: MonadIO m => Request -> Stream (Of BS.ByteString) m ()-}
{-streamingReq req = loop-}
  {-where-}
    {-go = liftIO (requestBody req)-}
    {-loop = do-}
      {-bs <- go-}
      {-liftIO $ print bs-}
      {-unless (BS.null bs) $ do-}
        {-liftIO $ print bs-}
        {-S.yield bs-}
        {-loop-}

instance ( KnownNat status
         ) => HasServer (StreamResponse method status contentTypes) ctx where
  type ServerT (StreamResponse method status contentTypes) m
    = m (Stream (Of BS.ByteString) (ResourceT IO) ())

  route _ _ctxt subapi = leafRouter $ \env request respond ->
    runAction subapi env request respond streamIt
    where

      streamIt :: Stream (Of BS.ByteString) (ResourceT IO) () -> RouteResult Response
      streamIt stream = Route $ streamingResponse (hoist runResourceT stream) status []

      status :: Status
      status = toEnum $ fromInteger $ natVal (Proxy :: Proxy status)
