{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Streaming.Client.Internal where

import Control.Monad
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString              as BS
import           Data.Proxy                   (Proxy (Proxy))
import qualified Network.HTTP.Media           as M
import           Servant.API                  hiding (Stream)
import           Servant.Client.Core
import           Servant.Streaming
import           Streaming
import qualified Streaming.Prelude            as S

instance (HasClient m subapi, RunClient m )
    => HasClient m (StreamBody contentTypes :> subapi) where
  type Client m (StreamBody contentTypes :> subapi)
    = (M.MediaType, Stream (Of BS.ByteString) (ResourceT IO) ())
      -> Client m subapi
  clientWithRoute pm _ req (mtype, body)
    = clientWithRoute
        pm
        (Proxy :: Proxy subapi)
        req { requestBody = Just (RequestBodyStreamChunked body', mtype) }
    where
      body' :: (IO BS.ByteString -> IO ()) -> IO ()
      body' write = runResourceT $ mapsM_ (go write) body

      go :: (IO BS.ByteString -> IO ()) -> Of BS.ByteString x -> ResourceT IO x
      go write (bs :> r) = do
        liftIO $ write (return bs)
        return r

instance (RunClient m )
    => HasClient m (StreamResponse verb status contentTypes) where
  type Client m (StreamResponse verb status contentTypes)
    = m (Stream (Of BS.ByteString) (ResourceT IO) ())
  clientWithRoute pm _ req = do
    resp <- runRequest req
    case responseBody resp of
      x -> _ -- return $ toStream $ responseBody resp
    where
      toStream :: IO BS.ByteString -> Stream (Of BS.ByteString) (ResourceT IO) ()
      toStream read = do
        bs <- liftIO read
        unless (BS.null bs) $ do
          S.yield bs
          toStream read
