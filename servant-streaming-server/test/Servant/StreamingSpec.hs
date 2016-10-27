module Servant.StreamingSpec (spec) where

import Servant
import Servant.Streaming.Server
import Test.Hspec

spec :: Spec
spec = do
  streamBodySpec

streamBodySpec :: Spec
streamBodySpec = describe "StreamBody instance" $ do

  it "streams the request body" pending
  it "passes as argument the content-type" pending
  it "responds with '415 - Unsupported Media Type' on wrong content type" pending

streamResponseSpec :: Spec
streamResponseSpec = describe "StreamResponse instance" $ do

  it "streams the response" pending
  it "sets the specified status code" pending
  it "responds with '405 - Method Not Allowed' on wrong method" pending
  it "responds with '406 - Not Acceptable' on wrong content type" pending

------------------------------------------------------------------------------
-- API

type API = StreamBody '[JSON] :> StreamResponseGet '[JSON]
