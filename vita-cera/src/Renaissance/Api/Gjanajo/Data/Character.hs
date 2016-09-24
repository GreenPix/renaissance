{-|
Module: Renaissance.Api.Gjanajo.Data.Character
Copyright: (c) GreenPix, 2016
License: MIT
Stability: experimental

This module provides the several data types used by the @gjanajo@ Api when
it comes to players characters.
-}

{-# LANGUAGE DeriveGeneric #-}

module Renaissance.Api.Gjanajo.Data.Character where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import GHC.Int
import Data.Time
import Data.UUID
import Data.UUID.Aeson

-- | The statistics of a given character, as used in @lycan@.
data Statistics = Statistics { level        :: Int
                             , strength     :: Int
                             , dexterity    :: Int
                             , constitution :: Int
                             , intelligence :: Int
                             , precision    :: Int
                             , wisdom       :: Int
                             }
  deriving (Generic)

-- | Automatically derived
instance FromJSON Statistics
-- | Automatically derived
instance ToJSON Statistics

-- | A data type to gather all the available information about one
-- character. The use of 'Maybe' allows an endpoint to include or not
-- include the related information. For instance, the statistics of one
-- character is not always required to be known.
data CharacterInformation = CharacterInformation { name        :: Text
                                                 , stats       :: Maybe Statistics
                                                 , skin        :: UUID
                                                 , accountUuid :: UUID
                                                 }
  deriving (Generic)

-- | Automatically derived
instance FromJSON CharacterInformation
-- | Automatically derived
instance ToJSON CharacterInformation
