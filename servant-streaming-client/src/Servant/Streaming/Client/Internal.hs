{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Servant.Streaming.Client.Internal where

import           Control.Monad
import           Control.Monad.Trans.Resource (ResourceT, getInternalState,
                                               runInternalState, runResourceT)
import qualified Data.ByteString              as BS
import           Data.IORef
import           Data.Proxy                   (Proxy (Proxy))
import qualified Network.HTTP.Media           as M
import           Servant.API                  hiding (Stream)
import           Servant.Client.Core
import           Servant.Streaming
import           Streaming
import qualified Streaming.Prelude            as S

instance (HasClient m subapi, RunClient m)
    => HasClient m (StreamBodyMonad contentTypes n :> subapi) where
  type Client m (StreamBodyMonad contentTypes n :> subapi)
    = (M.MediaType, Stream (Of BS.ByteString) (ResourceT IO) ())
      -> Client m subapi
  clientWithRoute pm _ req (mtype, body)
    = clientWithRoute
        pm
        (Proxy :: Proxy subapi)
        req { requestBody = Just (RequestBodyStreamChunked body', mtype) }
    where
      body' :: (IO BS.ByteString -> IO ()) -> IO ()
      body' write = runResourceT $ do
        ref <- liftIO $ newIORef body
        is <- getInternalState
        let popper :: IO BS.ByteString
            popper = do
              rsrc <- readIORef ref
              mres <- runInternalState (S.uncons rsrc) is
              case mres of
                Nothing -> return BS.empty
                Just (bs, str)
                  | BS.null bs -> writeIORef ref str >> popper
                  | otherwise -> writeIORef ref str >> return bs
        liftIO $ write popper
#if MIN_VERSION_servant_client_core(0,13,0)
  hoistClientMonad pm _ f cl = \a ->
    hoistClientMonad pm (Proxy :: Proxy subapi) f (cl a)
#endif

instance (RunClient m )
    => HasClient m (StreamResponse verb status contentTypes) where
  type Client m (StreamResponse verb status contentTypes)
    = m (Stream (Of BS.ByteString) (ResourceT IO) ())
  clientWithRoute _ _ req = do
    respStream <- runStreamingResponse <$> streamingRequest req
    let stream' = respStream responseBody
    return $ toStream stream'
    where
      toStream :: IO BS.ByteString -> Stream (Of BS.ByteString) (ResourceT IO) ()
      toStream read' = do
        bs <- liftIO read'
        liftIO $ print bs
        unless (BS.null bs) $ do
          S.yield bs
          toStream read'
#if MIN_VERSION_servant_client_core(0,13,0)
  hoistClientMonad _m _ f cl = f cl
#endif
