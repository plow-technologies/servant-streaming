-- | This module provides an instance for HasDocs for 'servant-streaming'
-- combinators, so that documentation can be generated for APIs that have
-- streaming request or response bodies.
--
-- As as convenience, it also re-exports the combinators themselves.
module Servant.Streaming.Docs
  ( StreamResponse
  , StreamBody
  , StreamResponseGet
  , StreamResponsePost
  , StreamResponsePut
  , StreamResponsePatch
  ) where

import Servant.Streaming
import Servant.Streaming.Docs.Internal ()
