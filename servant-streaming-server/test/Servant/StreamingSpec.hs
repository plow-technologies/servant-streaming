{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Servant.StreamingSpec (spec) where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Streaming.Char8 as BSS
import           Data.String               (fromString)
import           GHC.Stats
import qualified Network.HTTP.Media        as M
import qualified Pipes                     as Pipes
import           Pipes.HTTP                (ManagerSettings, Request,
                                            RequestBody, defaultManagerSettings,
                                            defaultRequest, responseTimeout,
                                            managerModifyRequest, method, path,
                                            port, requestBody, requestHeaders,
                                            responseBody, stream, withHTTP,
                                            withManager)
import qualified Pipes.Prelude             as Pipes
import           Servant
import           Servant.Streaming.Server
import           Streaming
import qualified Streaming.Prelude         as S
import           Test.Hspec

import Control.Concurrent
import Network.Wai.Handler.Warp

spec :: Spec
spec = do
  streamBodySpec
  streamResponseSpec

streamBodySpec :: Spec
streamBodySpec = describe "StreamBody instance" $ around withServer $ do

  it "streams the request body" $ \port -> do
    makeRequest port "length" (S.each ["hi"]) $ \responseStream -> do
      runResourceT (S.toList_ responseStream) `shouldReturn` ["2"]

  it "does not keep the request in memory" $ \port -> do
    makeRequest port "length" (getBytes $ 100) $ \responseStream -> do
      throwOut responseStream
      bytes <- maxBytesUsed <$> getGCStats
      bytes < 10 * megabyte `shouldBe` True

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
  =    "random" :> StreamResponseGet '[JSON]
  :<|> "length" :> StreamBody '[JSON] :> Post '[PlainText] Int
  :<|> "contentType" :> StreamBody '[JSON, PlainText] :> Post '[PlainText] M.MediaType
  :<|> "echo" :> StreamBody '[JSON] :> StreamResponsePost '[JSON]

api :: Proxy API
api = Proxy

server :: Server API
server = randomH :<|> lengthH :<|> contentTypeH :<|> echoH
  where
    randomH                          = return $ getBytes $ 10 * megabyte
    lengthH      _contentType stream = liftIO . runResourceT $ S.length_ stream
    contentTypeH contentType _stream = return contentType
    echoH        _contentType stream = return stream

withServer :: (Port -> IO ()) -> IO ()
withServer = testWithApplicationSettings settings (return $ serve api server)
  where
    settings = setTimeout 1000 defaultSettings

------------------------------------------------------------------------------
-- Utils

throwOut :: Stream (Of ByteString) (ResourceT IO) () -> IO ()
throwOut stream
  = runResourceT $ BSS.writeFile "/dev/null" $ BSS.fromChunks stream

getBytes :: Int -> Stream (Of ByteString) (ResourceT IO) ()
getBytes bytes = S.take bytes $ BSS.toChunks $ BSS.readFile "/dev/urandom"

makeRequest :: Port
            -> ByteString
            -> Stream (Of ByteString) (ResourceT IO) ()
            -> (Stream (Of ByteString) (ResourceT IO) () -> IO r)
            -> IO r
makeRequest appPort urlPath requestStream responseAction
  = withManager settings $ \manager ->
     withHTTP req manager $ \respPipe -> do
       responseAction (hoist liftIO $ S.unfoldr Pipes.next $ responseBody respPipe)
  where
    reqBody :: RequestBody
    reqBody = stream $ Pipes.unfoldr S.next $ hoist runResourceT requestStream

    req :: Request
    req = defaultRequest { port = appPort
                         , requestHeaders = [("Content-Type", "application/json")]
                         , method = "POST"
                         , path = urlPath
                         , requestBody = reqBody
                         , responseTimeout = Nothing
                         }

    settings :: ManagerSettings
    settings = defaultManagerSettings


instance Show a => MimeRender PlainText a where
  mimeRender _ = fromString . show

megabyte :: Num a => a
megabyte = 1000 ^ 2
