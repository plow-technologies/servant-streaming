module Servant.StreamingSpec (spec) where

import Servant
import Servant.Streaming.Server
import Test.Hspec

import Network.Wai.Handler.Warp

spec :: Spec
spec = do
  streamBodySpec

streamBodySpec :: Spec
streamBodySpec = describe "StreamBody instance" $ around withServer $ do

  it "streams the request body" $ \port -> pending
  it "passes as argument the content-type" $ \port -> pending
  it "responds with '415 - Unsupported Media Type' on wrong content type" $ \port -> pending

streamResponseSpec :: Spec
streamResponseSpec = describe "StreamResponse instance" $ do

  it "streams the response" pending
  it "sets the specified status code" pending
  it "responds with '405 - Method Not Allowed' on wrong method" pending
  it "responds with '406 - Not Acceptable' on wrong content type" pending

------------------------------------------------------------------------------
-- API

type API = StreamBody '[JSON] :> StreamResponseGet '[JSON]

api :: Proxy API
api = Proxy

server :: Server API
server contentType stream = _

withServer :: (Port -> IO ()) -> IO ()
withServer = testWithApplication (return $ serve api server)
