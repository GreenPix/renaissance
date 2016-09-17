{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Authent where

import Auth.Token
import Auth.Token.Persistent
import GHC.Int
import Servant
import Servant.Auth.Token.Api
import Servant.Auth.Token.Server
import Servant.Server.Experimental.Auth (AuthServerData)
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Renaissance.Api.Bz.Data

import Bz.Monad

type instance AuthServerData TokenProtect = Int64

getData :: Identity -> IO Int64
getData = return . fromSqlKey

getIdentityHandler :: TokenGetRouteBody -> BzM Identity
getIdentityHandler _ = return $ toSqlKey 1

whoAmIHandler :: Int64 -> BzM WhoAmIResponse
whoAmIHandler = return . WhoAmIResponse

newHandler :: BzM Int64
newHandler = do
    auth <- authenticator
    id <- liftIO $ newIdentity auth

    return $ fromSqlKey id

authentErrorHandler :: AuthError -> ServantErr
authentErrorHandler DisabledErr = err403 { errBody = "This account has been disabled" }
authentErrorHandler InvalidTokenErr = err403 { errBody = "Invalid token" }
authentErrorHandler UnknownIdErr = err403 { errBody = "Unknown user" }
