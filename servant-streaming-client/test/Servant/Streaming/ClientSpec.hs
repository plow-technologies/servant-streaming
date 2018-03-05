module Servant.Streaming.ClientSpec (spec) where

import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString              as BS
import           Data.String                  (fromString)
import           GHC.Stats
import           Network.HTTP.Client          (newManager, defaultManagerSettings)
import qualified Network.HTTP.Media           as M
import           Network.Wai.Handler.Warp
import           Servant                      ((:<|>) ((:<|>)), (:>), JSON,
                                               MimeRender (..), MimeUnrender (..), PlainText, Post,
                                               Proxy (..), Server, serve)
import           Servant.Client
import           Servant.Streaming.Client
import           Servant.Streaming.Server
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

  {-it "does not keep the request in memory" $ \port' -> do-}
    {-let req = streamReq port' "length"-}
            {-$ S.replicate megabyte-}
            {-$ BS.replicate 1000 97 -- 1000 MB total-}
    {-responseBody <$> makeRequest req-}
      {-`shouldReturn` fromString (show (1000 * megabyte :: Int))-}
    {-bytes <- max_live_bytes <$> getRTSStats-}
    {-bytes < 100 * megabyte `shouldBe` True-}

------------------------------------------------------------------------------
-- API

type API
  =    "length" :> StreamBody '[JSON] :> Post '[PlainText] Int
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

lengthC :<|> contentTypeC :<|> echoC
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
megabyte = 1000 ^ 2

------------------------------------------------------------------------------
-- Orphans

instance Show a => MimeRender PlainText a where
instance Read a => MimeUnrender PlainText a where

instance Read M.MediaType where
  {-read = M.parseAccept . T.pack-}
