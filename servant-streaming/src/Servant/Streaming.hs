module Servant.Streaming where

import GHC.TypeLits (Nat)
import Network.HTTP.Types

-- | A request body that should be streamed.
type StreamBody ct = StreamBodyMonad ct IO

-- | A request body that should be streamed with specified server monad
data StreamBodyMonad (contentTypes :: [*]) (m :: * -> *)

-- | A response body that should be streamed, with specified method, status,
-- and content-type.
data StreamResponse (method :: StdMethod) (status :: Nat)  (contentTypes :: [*])

-- | The streaming version of the @Get@ combinator.
type StreamResponseGet = StreamResponse 'GET 200

-- | The streaming version of the @Post@ combinator.
type StreamResponsePost = StreamResponse 'POST 200

-- | The streaming version of the @Put@ combinator.
type StreamResponsePut = StreamResponse 'PUT 200

-- | The streaming version of the @Patch@ combinator.
type StreamResponsePatch = StreamResponse 'PATCH 200
