{-# LANGUAGE DeriveGeneric #-}

module Renaissance.Api.Gjanajo.Data.Account where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import GHC.Int
import Data.Time
import Data.UUID.Aeson
import Data.UUID

data AccountInformation = AccountInformation { email    :: Text
                                             , lastSeen :: UTCTime
                                             , uuid     :: UUID
                                             }
  deriving (Generic)

instance FromJSON AccountInformation
instance ToJSON AccountInformation
