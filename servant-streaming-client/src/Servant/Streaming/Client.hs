-- | This module provides an instance for HasClient for 'servant-streaming'
-- combinators, so that client functions can be generated for APIs that have
-- streaming request or response bodies.
--
-- As as convenience, it also re-exports the combinators themselves.
module Servant.Streaming.Client
  ( StreamResponse
  , StreamBody
  , StreamResponseGet
  , StreamResponsePost
  , StreamResponsePut
  , StreamResponseDelete
  ) where

import Servant.Streaming
import Servant.Streaming.Client.Internal ()
