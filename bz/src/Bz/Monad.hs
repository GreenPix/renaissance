{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bz.Monad where

import Auth.Token.Persistent
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, ask, liftIO)
import Database.Persist.Sqlite (ConnectionPool)
import Servant.Client (BaseUrl, ServantError)
import Servant (ServantErr)
import Servant.Auth.Token.Server
import Servant.Server.Experimental.Auth (AuthServerData)
import Network.HTTP.Client (Manager)

data BzConfig = BzConfig { pool    :: ConnectionPool
                         , http    :: Manager
                         , gjanajo :: BaseUrl
                         }

type BzM = ReaderT BzConfig (ExceptT ServantErr IO)

queryGjanajo' :: (Manager -> BaseUrl -> ExceptT ServantError IO a)
              -> Manager
              -> BaseUrl
              -> IO (Either ServantError a)
queryGjanajo' query manager gj = liftIO $ runExceptT (query manager gj)

queryGjanajo :: (Manager -> BaseUrl -> ExceptT ServantError IO a)
             -> BzM (Either ServantError a)
queryGjanajo query = do manager <- http <$> ask
                        gj <- gjanajo <$> ask
                        liftIO $ queryGjanajo' query manager gj

instance AuthentMonad ConnectionPool BzM where
  authenticator = pool <$> ask
