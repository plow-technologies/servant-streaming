module Servant.Streaming where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat)
import Network.HTTP.Types
import Servant.API ((:>))
import Servant.Utils.Links (HasLink (..), Link, MkLink)

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

instance HasLink sub => HasLink (StreamBody ct :> sub) where
    type MkLink (StreamBody ct :> sub) r = MkLink sub r
    toLink toA _ = toLink toA (Proxy :: Proxy sub)

instance HasLink (StreamResponse method status ct) where
    type MkLink (StreamResponse method status ct) r = r
    toLink toA _ = toA
