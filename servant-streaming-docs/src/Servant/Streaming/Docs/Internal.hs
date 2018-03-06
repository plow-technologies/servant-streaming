{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Streaming.Docs.Internal where

import           Control.Lens
import           Data.Proxy               (Proxy (Proxy))
import           Servant.API              hiding (Stream)
import           Servant.API.ContentTypes (allMime, AllMime)
import           Servant.Docs
import           Servant.Streaming


instance (HasDocs subapi, AllMime contentTypes)
    => HasDocs (StreamBody contentTypes :> subapi) where
  docsFor _ (ep, action) opts
    = docsFor (Proxy :: Proxy subapi) (ep, action') opts
    where
      mimes = allMime (Proxy :: Proxy contentTypes)
      bodyFor x = ("Streaming Body", x, "--some streaming body--")

      action' = action & rqbody .~ (bodyFor <$> mimes)
                       & rqtypes .~ mimes
