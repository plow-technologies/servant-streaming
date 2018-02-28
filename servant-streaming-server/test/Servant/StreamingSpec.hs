{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Servant.StreamingSpec where

import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Streaming    as BSS
import           Data.String                  (fromString)
import           GHC.Stats
import qualified Network.HTTP.Media           as M
import           Network.Wai.Handler.Warp
import qualified Pipes                        as Pipes
import           Pipes.HTTP                   (Request,
                                               Response,
                                               defaultManagerSettings,
                                               defaultRequest, httpLbs, method,
                                               newManager, path, port,
                                               requestBody, requestHeaders,
                                               responseBody, responseTimeout,
                                               responseTimeoutNone, stream,
                                               withHTTP)
import qualified Pipes.Prelude                as Pipes
import           Servant                      ((:<|>) ((:<|>)), (:>), JSON,
                                               MimeRender (..), PlainText, Post,
                                               Proxy (..), Server, serve)
import           Servant.Streaming.Server
import           Streaming
import qualified Streaming.Prelude            as S
import           Test.Hspec


spec :: Spec
spec = do
  streamBodySpec
  streamResponseSpec

streamBodySpec :: Spec
streamBodySpec = describe "StreamBody instance" $ around withServer $ do

  it "streams the request body" $ \port' -> do
    let req = streamReq port' "length" (S.each ["h","i"])
    responseBody <$> makeRequest req `shouldReturn` "2"

  it "does not keep the request in memory" $ \port' -> do
    let req = streamReq port' "length"
            $ S.replicate megabyte
            $ BS.replicate 1000 97 -- 1000 MB total
    responseBody <$> makeRequest req
      `shouldReturn` fromString (show (1000 * megabyte :: Int))
    bytes <- max_live_bytes <$> getRTSStats
    bytes < 100 * megabyte `shouldBe` True

  it "passes as argument the content-type" $ \_port -> pending
  it "responds with '415 - Unsupported Media Type' on wrong content type" $ \_port -> pending

streamResponseSpec :: Spec
streamResponseSpec = describe "StreamResponse instance" $ around withServer $ do

  it "sets the specified status code" $ \_port -> pending
  it "responds with '405 - Method Not Allowed' on wrong method" $ \_port -> pending
  it "responds with '406 - Not Acceptable' on wrong content type" $ \_port -> pending

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

------------------------------------------------------------------------------
-- Utils

throwOut :: Stream (Of ByteString) (ResourceT IO) () -> IO ()
throwOut
  = runResourceT . BSS.stdout . BSS.fromChunks


makeRequest
  :: Request -> IO (Response BSL.ByteString)
makeRequest req = do
  manager <- newManager defaultManagerSettings
  httpLbs req manager

makeRequestStreamResponse
  :: Request -> (Stream (Of ByteString) (ResourceT IO) () -> IO r) -> IO (Response r)
makeRequestStreamResponse req responseAction = do
  manager <- newManager defaultManagerSettings
  withHTTP req manager $ \respPipe -> sequence $
    respPipe
      { responseBody =  responseAction
          (hoist liftIO $ S.unfoldr Pipes.next $ responseBody respPipe) }

streamReq
  :: Port -> ByteString -> Stream (Of ByteString) (ResourceT IO) () -> Request
streamReq appPort urlPath requestStream = defaultRequest
  { port = appPort
  , requestHeaders = [("Content-Type", "application/json")]
  , method = "POST"
  , path = urlPath
  , requestBody = streamReqBody requestStream
  , responseTimeout = responseTimeoutNone
  }
  where
    streamReqBody s
      = stream $ Pipes.unfoldr S.next $ hoist runResourceT s

instance Show a => MimeRender PlainText a where
  mimeRender _ = fromString . show

megabyte :: Num a => a
megabyte = 1000 ^ 2
