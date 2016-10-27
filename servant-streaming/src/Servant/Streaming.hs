module Servant.Streaming where

import GHC.TypeLits (Nat)
import Network.HTTP.Types

data StreamBody (contentTypes :: [*])
data StreamResponse (method :: StdMethod) (status :: Nat)  (contentTypes :: [*])

type StreamResponseGet = StreamResponse 'GET 200
type StreamResponsePost = StreamResponse 'POST 200
type StreamResponsePut = StreamResponse 'PUT 200
type StreamResponseDelete = StreamResponse 'DELETE 200
