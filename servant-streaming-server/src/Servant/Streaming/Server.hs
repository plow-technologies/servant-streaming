{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Streaming.Server where

import           Control.Monad.IO.Class
import qualified Data.ByteString                            as BS
import           Data.Maybe                                 (fromMaybe)
import qualified Network.HTTP.Media                         as M
import           Network.HTTP.Types                         (hContentType)
import           Network.Wai                                (Request,
                                                             requestBody,
                                                             requestHeaders)
import           Servant
import           Servant.Server.Internal.RoutingApplication (DelayedIO,
                                                             addBodyCheck,
                                                             delayedFailFatal,
                                                             withRequest)
import           Servant.Streaming
import           Streaming
import qualified Streaming.Prelude                          as S

instance HasServer subapi ctx => HasServer (StreamBody contentTypes :> subapi) ctx where
  type ServerT (StreamBody contentTypes :> subapi) m
    = M.MediaType -> Stream (Of BS.ByteString) m () -> ServerT subapi m

  route _ ctxt subapi =
    route (Proxy :: Proxy subapi) ctxt
      $ addBodyCheck (addBodyCheck subapi getContentType) makeBody
      where
        getContentType :: DelayedIO M.MediaType
        getContentType = withRequest $ \request -> do
          let contentTypeHdr
               = fromMaybe ("application" M.// "octet-stream")
               $ lookup hContentType (requestHeaders request) >>= M.parseAccept
          if contentTypeHdr `elem` contentTypeList
            then return contentTypeHdr else delayedFailFatal err415

        contentTypeList :: [M.MediaType]
        contentTypeList = _

        makeBody :: MonadIO m => DelayedIO (Stream (Of BS.ByteString) m ())
        makeBody = withRequest $ return . sourceRequestBody

        sourceRequestBody :: MonadIO m => Request -> Stream (Of BS.ByteString) m ()
        sourceRequestBody req = S.untilRight go
          where
            go = do
              bs <- liftIO (requestBody req)
              return $ if (BS.null bs) then Left bs else Right ()

{-
instance HasServer (StreamResponse method status ctyp) ctx where
  type ServerT (StreamBody :> subapi) m = Stream (Of BS.ByteString) m ()

  route _ ctxt subapi =
    route (Proxy :: Proxy subapi) _
    -}
