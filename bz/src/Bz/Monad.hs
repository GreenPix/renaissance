{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bz.Monad where

import Auth.Token.Persistent
import Control.Monad.Except
import Control.Monad.Reader
import Database.Persist.Sqlite (ConnectionPool)
import Renaissance.Api.Bz
import Servant
import Servant.Auth.Token.Server
import Servant.Server.Experimental.Auth (AuthServerData)

data BzConfig = BzConfig
              { pool :: ConnectionPool }

type BzM = ReaderT BzConfig (ExceptT ServantErr IO)

instance AuthentMonad ConnectionPool BzM where
  authenticator = pool <$> ask
