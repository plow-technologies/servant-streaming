{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Streaming.ClientSpec (spec) where

import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC
import qualified Data.ByteString.Lazy.Char8   as BSCL
import           GHC.Stats
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import qualified Network.HTTP.Media           as M
import           Network.Wai.Handler.Warp
import           Servant                      ((:<|>) ((:<|>)), (:>), JSON,
                                               MimeRender (..),
                                               MimeUnrender (..), PlainText,
                                               Post, Proxy (..), Server, serve)
import           Servant.Client
import           Servant.Streaming.Client
import           Servant.Streaming.Server     ()
import           Streaming
import qualified Streaming.Prelude            as S
import           Test.Hspec

spec :: Spec
spec = do
  streamBodySpec
  {-streamResponseSpec-}

streamBodySpec :: Spec
streamBodySpec = describe "StreamBody instance" $ around withServer $ do

  it "streams the request body" $ \port' -> do
    runClient port' (lengthC ("application" M.// "json", S.each ["h","i"]))
      `shouldReturn` Right 2

  it "does not keep the request in memory" $ \port' -> do
    let bd = S.replicate 1000 (BSC.replicate megabyte 'a') -- 1000 MB total
    runClient port' (lengthC ("application" M.// "json", bd))
      `shouldReturn` Right (1000 * megabyte)
    bytes <- max_live_bytes <$> getRTSStats
    bytes < 100 * megabyte `shouldBe` True

------------------------------------------------------------------------------
-- API

type API
  =    "length" :> StreamBody '[JSON] :> Post '[JSON] Int
  :<|> "contentType" :> StreamBody '[JSON, PlainText] :> Post '[PlainText] M.MediaType
  :<|> "echo" :> StreamBody '[JSON] :> StreamResponsePost '[JSON]

api :: Proxy API
api = Proxy

server :: Server API
server = lengthH :<|> contentTypeH :<|> echoH
  where
    lengthH      (_contentType, stream')
      = liftIO . runResourceT $ S.sum_ $ S.subst (\x -> BS.length x :> ()) stream'
    contentTypeH (contentType, _stream')
      = return contentType
    echoH        (_contentType, stream')
      = return stream'

withServer :: (Port -> IO ()) -> IO ()
withServer = testWithApplicationSettings settings (return $ serve api server)
  where
    settings = setTimeout 1000 defaultSettings

lengthC
  :: (M.MediaType, Stream (Of BS.ByteString) (ResourceT IO) ())
  -> ClientM Int
_contentTypeC
  :: (M.MediaType, Stream (Of BS.ByteString) (ResourceT IO) ())
  -> ClientM M.MediaType
_echoC
  :: (M.MediaType, Stream (Of BS.ByteString) (ResourceT IO) ())
  -> ClientM (Stream (Of BS.ByteString) (ResourceT IO) ())
lengthC :<|> _contentTypeC :<|> _echoC
  = client api

------------------------------------------------------------------------------
-- Utils

runClient  :: Port -> ClientM a -> IO (Either ServantError a)
runClient p action = do
  mgr <- newManager defaultManagerSettings
  let env = ClientEnv
        { manager = mgr
        , baseUrl = BaseUrl Http "localhost" p ""
        , cookieJar = Nothing
        }
  runClientM action env

megabyte :: Num a => a
megabyte = 1000 ^ (2 :: Int)

------------------------------------------------------------------------------
-- Orphans

instance Show a => MimeRender PlainText a where
  mimeRender _ = BSCL.pack . show
instance Read a => MimeUnrender PlainText a where
  mimeUnrender _ = error . BSCL.unpack

instance Read M.MediaType where
  readsPrec _ x = case M.parseAccept (BSC.pack x) of
    Nothing -> error $ show x
    Just y -> [(y, "")]
