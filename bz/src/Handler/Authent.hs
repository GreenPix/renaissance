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
import Bz.Model

type instance AuthServerData TokenProtect = UUID

getData :: BzConfig -> Identity -> IO UUID
getData conf id = do
    u <- runSqlPool (getUuid id) (pool conf)
    case u of Just u -> do res <- queryGjanajo' (accountsByUuid u) (http conf) (gjanajo conf)
                           case res of Right acc -> return $ uuid acc
                                       Left  err -> do fail $ "error: " ++ (show err)
              _ -> fail $ "error: no uuid found"

-- TODO use the email to find the correct user
getIdentityHandler :: TokenGetRouteBody -> BzM Identity
getIdentityHandler (TokenGetRouteBody _) = return $ toSqlKey 1

whoAmIHandler :: UUID -> BzM WhoAmIResponse
whoAmIHandler = return . WhoAmIResponse

newHandler :: BzM UUID
newHandler = do
    auth <- authenticator
    res <- queryGjanajo accountsNewPost

    case res of Right uuid -> do id <- liftIO $ newIdentity auth
                                 runQuery $ newLink id uuid

                                 return uuid
                Left err   -> throwError err409 { errBody = "Cannot create new account for unknown reason. Try later" }

authentErrorHandler :: AuthError -> ServantErr
authentErrorHandler DisabledErr = err403 { errBody = "This account has been disabled" }
authentErrorHandler InvalidTokenErr = err403 { errBody = "Invalid token" }
authentErrorHandler UnknownIdErr = err403 { errBody = "Unknown user" }
