{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Accounts where

import Renaissance.Api.Gjanajo.Data.Account (AccountInformation(..))
import Gjanajo.Monad (GjanajoM, GjanajoConfig(..))
import Data.UUID (UUID)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Servant (NoContent, throwError, err501)
import System.Random (randomIO)

getAccounts :: Maybe Int
            -> Maybe Int
            -> GjanajoM [AccountInformation]
getAccounts _ _ = return []

getAccountsByUUID :: UUID
                  -> GjanajoM AccountInformation
getAccountsByUUID uuid = do now <- liftIO getCurrentTime 
                            return $ AccountInformation "Test" now uuid

deleteAccountsByUUID :: UUID
                     -> GjanajoM NoContent
deleteAccountsByUUID _ = throwError $ err501

postAccountsNew :: GjanajoM UUID
postAccountsNew = liftIO randomIO
