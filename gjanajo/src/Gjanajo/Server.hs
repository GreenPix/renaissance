{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Gjanajo.Server where

import Data.Proxy
import Handler.Accounts (getAccounts, getAccountsByUUID, deleteAccountsByUUID, postAccountsNew)
import Gjanajo.Monad (GjanajoM, GjanajoConfig)
import Servant ((:<|>) ((:<|>)), (:~>) (Nat), Server, ServerT, ServantErr, enter, serve)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (runReaderT)

import Renaissance.Api.Gjanajo

authToExcept :: GjanajoConfig
             -> GjanajoM
             :~> ExceptT ServantErr IO
authToExcept pool = Nat $ \x -> runReaderT x pool

authServe :: GjanajoConfig
          -> ServerT GjanajoApi GjanajoM
          -> Server GjanajoApi
authServe pool = enter $ authToExcept pool


server :: ServerT GjanajoApi GjanajoM
server = getAccounts :<|> getAccountsByUUID :<|> deleteAccountsByUUID :<|> postAccountsNew

gjanajoApi :: Proxy GjanajoApi
gjanajoApi = Proxy

gjanajod :: GjanajoConfig
         -> Application
gjanajod conf = logStdoutDev $ serve gjanajoApi (authServe conf server)
