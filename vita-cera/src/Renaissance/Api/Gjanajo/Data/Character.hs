{-# LANGUAGE DeriveGeneric #-}

module Renaissance.Api.Gjanajo.Data.Character where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import GHC.Int
import Data.Time
import Data.UUID
import Data.UUID.Aeson

data Statistics = Statistics { level        :: Int
                             , strength     :: Int
                             , dexterity    :: Int
                             , constitution :: Int
                             , intelligence :: Int
                             , precision    :: Int
                             , wisdom       :: Int
                             }
  deriving (Generic)

instance FromJSON Statistics
instance ToJSON Statistics

data CharacterInformation = CharacterInformation { name        :: Text
                                                 , stats       :: Maybe Statistics
                                                 , skin        :: UUID
                                                 , accountUuid :: UUID
                                                 }
  deriving (Generic)

instance FromJSON CharacterInformation
instance ToJSON CharacterInformation
