{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Renaissance.Api.Bz
  ( module Renaissance.Api.Bz.Data
  , ForeignBzApi
  , BzApi
  , BzPublicApi
  ) where

import Servant.API
import Servant.Auth.Token.Api
import GHC.Int

import Renaissance.Api.Bz.Data

type ForeignBzApi = ForeignAuthentApi TokenGetRouteBody WhoAmIResponse
               :<|> BzPublicApi
            -- :<|> TokenProtect :> BzPrivateApi

type BzApi = AuthentApi TokenGetRouteBody WhoAmIResponse
        :<|> BzPublicApi

type BzPublicApi = "accounts" :> "new" :> PostCreated '[JSON] Int64
