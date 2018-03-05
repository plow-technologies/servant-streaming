module Servant.Streaming where

import GHC.TypeLits (Nat)
import Network.HTTP.Types

-- | A request body that should be streamed.
data StreamBody (contentTypes :: [*])

-- | A response body that should be streamed, with specified method, status,
-- and content-type.
data StreamResponse (method :: StdMethod) (status :: Nat)  (contentTypes :: [*])

type StreamResponseGet = StreamResponse 'GET 200
type StreamResponsePost = StreamResponse 'POST 200
type StreamResponsePut = StreamResponse 'PUT 200
type StreamResponsePatch = StreamResponse 'PATCH 200
