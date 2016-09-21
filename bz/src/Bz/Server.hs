{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Bz.Server where

import Control.Monad.Except
import Control.Monad.Reader
import Database.Persist.Sqlite
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Auth.Token.Server

import Renaissance.Api.Bz

import Bz.Monad
import Handler.Authent

authToExcept :: BzConfig
             -> BzM
             :~> ExceptT ServantErr IO
authToExcept pool = Nat $ \x -> runReaderT x pool

authServe :: BzConfig
          -> ServerT BzApi BzM
          -> Server BzApi
authServe pool = enter $ authToExcept pool

server :: ServerT BzApi BzM
server = (mkAuthServer getIdentityHandler whoAmIHandler authentErrorHandler)
    :<|> newHandler

bzApi :: Proxy BzApi
bzApi = Proxy

bzd :: BzConfig
    -> Application
bzd conf = logStdoutDev $ serveWithContext bzApi
                                           (authTokenContext (pool conf) (getData conf) authentErrorHandler)
                                           (authServe conf server)
