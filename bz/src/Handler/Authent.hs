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
import Data.UUID (UUID)
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import System.Random (randomIO)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ServantError)

import Renaissance.Api.Bz.Data
import Renaissance.Api.Gjanajo.Data.Account (AccountInformation, uuid)
import Renaissance.Client.Gjanajo (accountsByUuid, accountsNewPost)

import Bz.Monad

type instance AuthServerData TokenProtect = UUID

getData :: Manager -> BaseUrl -> Identity -> IO UUID
getData manager base id = do
    -- TODO read uuid in db
    u <- randomIO
    res <- queryGjanajo' (accountsByUuid u) manager base
    case res of Right acc -> return $ uuid acc
                Left  err -> do fail $ "error: " ++ (show err)

getIdentityHandler :: TokenGetRouteBody -> BzM Identity
getIdentityHandler _ = return $ toSqlKey 1

whoAmIHandler :: UUID -> BzM WhoAmIResponse
whoAmIHandler = return . WhoAmIResponse

newHandler :: BzM UUID
newHandler = do
    auth <- authenticator
    res <- queryGjanajo accountsNewPost

    case res of Right uuid -> do id <- liftIO $ newIdentity auth
                                 -- TODO store link id,uuid in db
                                 return uuid
                Left err   -> throwError err409 { errBody = "Cannot create new account for unknown reason. Try later" }

authentErrorHandler :: AuthError -> ServantErr
authentErrorHandler DisabledErr = err403 { errBody = "This account has been disabled" }
authentErrorHandler InvalidTokenErr = err403 { errBody = "Invalid token" }
authentErrorHandler UnknownIdErr = err403 { errBody = "Unknown user" }
