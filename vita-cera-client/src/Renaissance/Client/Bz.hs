{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Renaissance.Client.Bz
  ( postAccountsNew
  , postTokenGet
  , postTokenRefresh
  , getWhoAmI
  ) where

import Servant.Auth.Token.Api (TokenProtect)
import Auth.Token (AccessGrant, AccessToken)
import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy (Proxy(..))
import Data.Token (toText)
import Data.UUID (UUID)
import Network.HTTP.Client (Manager)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Auth.Token.Api (TokenProtect, PostTokenRefreshReq)
import Servant.Client (client, BaseUrl, ServantError, AuthenticateReq)

import Renaissance.Api.Base
import Renaissance.Api.Bz (TokenGetRouteBody, BzApi, WhoAmIResponse)

bzApi :: Proxy BzApi
bzApi = Proxy

postAccountsNew :: Manager
                -> BaseUrl
                -> ExceptT ServantError IO UUID

postTokenGet :: TokenGetRouteBody
                -> Manager
                -> BaseUrl
                -> ExceptT ServantError IO AccessGrant

postTokenRefresh :: PostTokenRefreshReq
                 -> Manager
                 -> BaseUrl
                 -> ExceptT ServantError IO AccessGrant

getWhoAmI :: AuthenticateReq TokenProtect
          -> Manager
          -> BaseUrl
          -> ExceptT ServantError IO WhoAmIResponse

((postTokenGet :<|> postTokenRefresh) :<|> getWhoAmI) :<|> postAccountsNew = client bzApi
