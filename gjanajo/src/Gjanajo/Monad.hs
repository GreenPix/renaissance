{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gjanajo.Monad where

import Database.Persist.Sqlite (ConnectionPool)
import Servant (ServantErr)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)

data GjanajoConfig = GjanajoConfig { pool    :: ConnectionPool
                                   }

type GjanajoM = ReaderT GjanajoConfig (ExceptT ServantErr IO)
