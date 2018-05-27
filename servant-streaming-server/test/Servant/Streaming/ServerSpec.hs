{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Servant.Streaming.ServerSpec (spec) where

import           Control.Concurrent
import           Control.Exception            (bracket)
import           Control.Monad.Trans.Resource (ResourceT, getInternalState,
                                               runInternalState, runResourceT)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Streaming    as BSS
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL

import           Data.IORef
import           Data.String                  (fromString, IsString)
import           GHC.Stats
import qualified Network.HTTP.Media           as M
import           Network.HTTP.Types           (status200, status405, status406,
                                               status415)
import           Network.Wai.Handler.Warp
import           Pipes.HTTP                   (Request, RequestBody (..),
                                               Response, brRead,
                                               defaultManagerSettings,
                                               defaultRequest, httpLbs, method,
                                               newManager, path, port,
                                               requestBody, requestHeaders,
                                               responseBody, responseStatus,
                                               responseTimeout,
                                               responseTimeoutNone,
                                               withResponse)
import           Servant                      ((:<|>) ((:<|>)), (:>), JSON,
                                               MimeRender (..), PlainText, Post,
                                               Proxy (..), Server, serve)
import           Servant.Streaming.Server
import           Streaming
import qualified Streaming.Prelude            as S
import           System.Directory             (removeFile)
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
    bytes < 200 * megabyte `shouldBe` True

  it "passes as argument the content-type" $ \port' -> do
    let req = streamReq port' "contentType" (S.each ["h","i"])
    responseBody <$> makeRequest req `shouldReturn` "application/json"
    let req' = req { requestHeaders = [("Content-Type", "text/plain;charset=utf-8")] }
    responseBody <$> makeRequest req' `shouldReturn` "text/plain;charset=utf-8"

  it "responds with '415 - Unsupported Media Type' on wrong content type" $ \port' -> do
    let req' = streamReq port' "length" (S.each ["h","i"])
        req = req' { requestHeaders = [("Content-Type", "bla/bla")] }
    responseStatus <$> makeRequest req `shouldReturn` status415

streamResponseSpec :: Spec
streamResponseSpec = describe "StreamResponse instance" $ around withServer $ do

  it "streams the response body" $ \port' -> do
    let req = streamReq port' "echo" (S.each ["h","i"])
    responseBody <$> makeRequest req `shouldReturn` "hi"

  it "does not keep the response in memory" $ \port' -> do
    let req = streamReq port' "echo"
            $ S.replicate 100
            $ BS.replicate megabyte 97 -- 100 MB total
    _ <- makeRequestStreamResponse req (runResourceT . S.length)
    bytes <- max_live_bytes <$> getRTSStats
    bytes < 200 * megabyte `shouldBe` True

  it "sets the specified status code" $ \port' -> do
    let req = streamReq port' "length" (S.each ["h","i"])
    responseStatus <$> makeRequest req `shouldReturn` status200

  it "responds with '405 - Method Not Allowed' on wrong method" $ \port' -> do
    let req' = streamReq port' "echo" (S.each ["h","i"])
        req = req' { method = "GET" }
    responseStatus <$> makeRequest req `shouldReturn` status405

  it "responds with '406 - Not Acceptable' on wrong content type" $ \port' -> do
    let req' = streamReq port' "echo" (S.each ["h","i"])
        req = req' { requestHeaders = ("Accept", "bla/bla"):requestHeaders req' }
    responseStatus <$> makeRequest req `shouldReturn` status406

  it "handles resource deallocation correctly" $ \port' -> do
    let req' = streamReq port' "getfile" (S.each []) -- TODO: simplify
        contents :: IsString a => a
        contents = "foobar"
    x <- bracket
      (BS.writeFile "hello.txt" contents)
      (\_ -> removeFile "hello.txt")
      (\_ -> responseBody <$> makeRequest req')
    x `shouldBe` fromString contents


------------------------------------------------------------------------------
-- API

type API
  =    "length" :> StreamBody '[JSON] :> Post '[PlainText] Int
  :<|> "contentType" :> StreamBody '[JSON, PlainText] :> Post '[PlainText] M.MediaType
  :<|> "echo" :> StreamBody '[JSON] :> StreamResponsePost '[JSON]
  :<|> "getfile" :> StreamResponsePost '[PlainText]

api :: Proxy API
api = Proxy

server :: Server API
server = lengthH :<|> contentTypeH :<|> echoH :<|> getfileH
  where
    lengthH      (_contentType, stream')
      = liftIO . runResourceT $ S.sum_ $ S.subst (\x -> BS.length x :> ()) stream'
    contentTypeH (contentType, _stream')
      = return contentType
    echoH        (_contentType, stream')
      = return stream'
    getfileH = return $ BSS.toChunks (BSS.readFile "hello.txt")

withServer :: (Port -> IO ()) -> IO ()
withServer = withApplicationSettings settings (return $ serve api server)
  where
    settings = setTimeout 60 defaultSettings

------------------------------------------------------------------------------
-- Utils

makeRequest
  :: Request -> IO (Response BSL.ByteString)
makeRequest req = do
  manager <- newManager defaultManagerSettings
  httpLbs req manager

makeRequestStreamResponse
  :: Request -> (Stream (Of ByteString) (ResourceT IO) () -> IO r) -> IO r
makeRequestStreamResponse req responseAction = do
  manager <- newManager defaultManagerSettings
  withResponse req manager (responseAction . go . responseBody)
  where
    go :: MonadIO m => IO BS.ByteString -> Stream (Of ByteString) m ()
    go respReader = do
      n <- liftIO $ brRead respReader
      if BS.null n
      then return ()
      else do
        S.yield n
        go respReader

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
    streamReqBody body
      = RequestBodyStreamChunked body'
      where
        body' :: (IO BS.ByteString -> IO ()) -> IO ()
        body' write = void . forkIO . runResourceT $ do
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

instance Show a => MimeRender PlainText a where
  mimeRender _ = fromString . show

megabyte :: Num a => a
megabyte = 1000 ^ 2
