{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Renaissance.Api.Bz
  ( module Renaissance.Api.Bz.Data
  , ForeignBzApi
  , BzApi
  , BzPublicApi
  , WithAuthToken
  ) where

import Servant.API
import Servant.Auth.Token.Api
import Data.UUID (UUID)
import Data.Text (Text)

import Renaissance.Api.Bz.Data

type WithAuthToken a = Headers '[Header "authorization-token" AuthorizationToken] a

type ForeignBzApi = ForeignAuthentApi TokenGetRouteBody WhoAmIResponse
               :<|> BzPublicApi
            -- :<|> TokenProtect :> BzPrivateApi

type BzApi = AuthentApi TokenGetRouteBody WhoAmIResponse
        :<|> BzPublicApi

type BzPublicApi =
       "accounts" :> "new" :> "dev" :> Capture "name" Text :> PostCreated '[JSON] (WithAuthToken NoContent)
  :<|> "auth" :> "dev" :> Capture "name" Text :> PostAccepted '[JSON] (WithAuthToken NoContent)
