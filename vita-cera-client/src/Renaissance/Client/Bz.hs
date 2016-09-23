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
import Data.Text (Text)
import Data.UUID (UUID)
import Network.HTTP.Client (Manager)
import Servant.API ((:<|>) ((:<|>)), Headers, NoContent)
import Servant.Auth.Token.Api (TokenProtect, PostTokenRefreshReq)
import Servant.Client (client, BaseUrl, ServantError, AuthenticateReq, ClientM)

import Renaissance.Api.Base
import Renaissance.Api.Bz (TokenGetRouteBody, BzApi, WhoAmIResponse, WithAuthToken)

bzApi :: Proxy BzApi
bzApi = Proxy

postAccountsNew :: Text
                -> Manager
                -> BaseUrl
                -> ClientM (WithAuthToken NoContent)

postAuthDev :: Text
            -> Manager
            -> BaseUrl
            -> ClientM (WithAuthToken NoContent)

postTokenGet :: TokenGetRouteBody
                -> Manager
                -> BaseUrl
                -> ClientM AccessGrant

postTokenRefresh :: PostTokenRefreshReq
                 -> Manager
                 -> BaseUrl
                 -> ClientM AccessGrant

getWhoAmI :: AuthenticateReq TokenProtect
          -> Manager
          -> BaseUrl
          -> ClientM WhoAmIResponse

((postTokenGet :<|> postTokenRefresh) :<|> getWhoAmI) :<|> (postAuthDev :<|> postAccountsNew) = client bzApi
