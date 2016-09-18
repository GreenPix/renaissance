{-# LANGUAGE TypeFamilies #-}

module Renaissance.Client.Base where

import Servant.Auth.Token.Api (TokenProtect)
import Servant.Client (AuthClientData)
import Auth.Token (AccessToken)
import Servant.Common.Req (Req, addHeader)
import Data.Token (toText)

-- TODO this should be in a servant-auth-token-client package
type instance AuthClientData TokenProtect = AccessToken

authenticateReq :: AccessToken -> Req -> Req
authenticateReq s req = addHeader "auth-token" (toText s) req
