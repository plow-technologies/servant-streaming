{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Servant.StreamingSpec (spec) where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Streaming as BSS
import           Data.String               (fromString)
import           GHC.Stats
import qualified Network.HTTP.Media        as M
import qualified Pipes                     as Pipes
import           Pipes.HTTP                (defaultManagerSettings,
                                            defaultRequest,
                                            managerModifyRequest, method, port,
                                            requestBody, requestHeaders,
                                            responseBody, withHTTP, withManager)
import           Servant
import           Servant.Streaming.Server
import           Streaming
import qualified Streaming.Prelude         as S
import           Test.Hspec

import Control.Concurrent
import Network.Wai.Handler.Warp

spec :: Spec
spec = do
  jointSpec
  streamBodySpec
  streamResponseSpec

jointSpec :: Spec
jointSpec = describe "StreamBody/StreamResponse" $ around withServer $ do

  it "streams the request body" $ \port -> do
    makeRequest port (S.each ["hi"]) $ \responseStream -> do
      print port
      threadDelay 100000000
      runResourceT (S.toList_ responseStream) `shouldReturn` ["hi"]

  it "does not keep the request in memory" $ \port -> do
    makeRequest port (getBytes gigabyte) $ \responseStream -> do
      throwOut responseStream
      bytes <- maxBytesUsed <$> getGCStats
      bytes < 10 * megabyte `shouldBe` True

streamBodySpec :: Spec
streamBodySpec = describe "StreamBody instance" $ around withServer $ do

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
    randomH                          = return $ getBytes gigabyte
    lengthH      _contentType stream = liftIO . runResourceT $ S.length_ stream
    contentTypeH contentType _stream = return contentType
    echoH        _contentType stream = return stream

withServer :: (Port -> IO ()) -> IO ()
withServer = testWithApplication (return $ serve api server)

------------------------------------------------------------------------------
-- Utils

throwOut :: Stream (Of ByteString) (ResourceT IO) () -> IO ()
throwOut stream
  = runResourceT $ BSS.writeFile "/dev/null" $ BSS.fromChunks stream

getBytes :: Int -> Stream (Of ByteString) (ResourceT IO) ()
getBytes bytes = S.take bytes $ BSS.toChunks $ BSS.readFile "/dev/urandom"

makeRequest :: Port
            -> Stream (Of ByteString) (ResourceT IO) ()
            -> (Stream (Of ByteString) (ResourceT IO) () -> IO r)
            -> IO r
makeRequest appPort requestStream responseAction
  = withManager settings $ \manager ->
     withHTTP req manager $ \respPipe -> do
       responseAction (hoist liftIO $ S.unfoldr Pipes.next $ responseBody respPipe)
  where
    req = defaultRequest { port = appPort
                         , requestHeaders = [("Content-Type", "application/json")]
                         , method = "POST"
                         }
    settings = defaultManagerSettings {
        managerModifyRequest = \req -> print req >> return req }

instance Show a => MimeRender PlainText a where
  mimeRender _ = fromString . show

gigabyte :: Num a => a
gigabyte = 1000 ^ 3

megabyte :: Num a => a
megabyte = 1000 ^ 2
