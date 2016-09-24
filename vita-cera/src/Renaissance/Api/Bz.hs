{-|
Module: Renaissance.Api.Bz
Copyright: (c) GreenPix, 2016
License: MIT
Stability: experimental

@bz@ server is the authentication server of the Renaissance project. It
provides access and refresh tokens to client after it verifies their
authentication information.
-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Renaissance.Api.Bz
  ( -- * Bz Api Types
    ForeignBzApi
  , BzApi
  , BzPublicApi
  -- * Module re-export
  ,  module Renaissance.Api.Bz.Data
  ) where

import Servant.API
import Servant.Auth.Token.Api
import Data.UUID

import Renaissance.Api.Bz.Data

-- | The 'ForeignBzApi' type is a “fake” API specification which does not
-- use the experimental servant authentication combinator. In doing so, it
-- can be used by a servant-foreign user to generate client functions.
type ForeignBzApi = ForeignAuthentApi TokenGetRouteBody WhoAmIResponse
               :<|> BzPublicApi
            -- :<|> TokenProtect :> BzPrivateApi

-- | The 'BzApi' is the specification of @bz@ rest API. It provides several
-- routes, including the ones provided by the @servant-auth-token@
-- packages.
type BzApi = AuthentApi TokenGetRouteBody WhoAmIResponse
        :<|> BzPublicApi

-- | The 'BzPublicApi' gathers the unprotected endpoints of the API.
-- Currently, it only provides the @accounts/new@ endpoint.
type BzPublicApi = "accounts" :> "new" :> PostCreated '[JSON] UUID
