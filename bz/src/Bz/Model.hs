{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Bz.Model where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Data.UUID (UUID, toText, fromText)
import Auth.Token.Persistent (Identity)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import Control.Monad (void)

import UUID

share [mkPersist sqlSettings, mkMigrate "bzMigrateAuth"]
  $(persistFileWith lowerCaseSettings "bz.model")

newLink :: (MonadBaseControl IO m, MonadIO m, Monad m)
        => Identity
        -> UUID
        -> SqlPersistT m ()
newLink id uuid = do void $ insert $ TokenOwner id uuid

getUuid :: (MonadBaseControl IO m, MonadIO m, Monad m)
        => Identity
        -> SqlPersistT m (Maybe UUID)
getUuid id = do res <- getBy (UniqueIdentity id)
                case res of Just (Entity _ val) -> return $ Just $ tokenOwnerOwner val
                            _                   -> return Nothing

getIdentity :: (MonadBaseControl IO m, MonadIO m, Monad m)
            => UUID
            -> SqlPersistT m (Maybe Identity)
getIdentity uuid = do res <- getBy (UniqueOwner uuid)
                      case res of Just (Entity _ val) -> return $ Just $ tokenOwnerIdentity val
                                  _                   -> return Nothing
