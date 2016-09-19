{-# LANGUAGE DeriveGeneric #-}

module Renaissance.Api.Bz.Data where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.UUID.Aeson
import Data.UUID

data TokenGetRouteBody = TokenGetRouteBody
                       { email :: Text }
  deriving (Generic)

instance FromJSON TokenGetRouteBody
instance ToJSON TokenGetRouteBody
  
data WhoAmIResponse = WhoAmIResponse
                    { id :: UUID }
  deriving (Generic)

instance FromJSON WhoAmIResponse
instance ToJSON WhoAmIResponse
