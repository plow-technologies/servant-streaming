-- | This module provides an instance for HasServant for 'servant-streaming'
-- combinators, so that servers with streaming (requests or responses) may be
-- used.
--
-- As as convenience, it also re-exports the combinators themselves.
module Servant.Streaming.Server
  ( StreamResponse
  , StreamBody
  , StreamResponseGet
  , StreamResponsePost
  , StreamResponsePut
  , StreamResponsePatch
  ) where

import Servant.Streaming
import Servant.Streaming.Server.Internal ()
