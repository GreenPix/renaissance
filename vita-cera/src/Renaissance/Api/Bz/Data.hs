{-|
Module: Renaissance.Api.Bz.Data
Copyright: (c) GreenPix, 2016
License: MIT
Stability: experimental

This module provides the several data types used by the @bz@ Api. They are
re-exported for convenience in the 'Renaissance.Api.Bz' module.
-}
{-# LANGUAGE DeriveGeneric #-}

module Renaissance.Api.Bz.Data where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.UUID.Aeson
import Data.UUID

-- | The @token/get@ route body
data TokenGetRouteBody = TokenGetRouteBody
                       { email :: Text
                       -- ^ The email is currently used as an unique
                       -- identifier for the accounts, but it will change
                       -- in the future.
                       }
  deriving (Generic)

-- | Automatically derived
instance FromJSON TokenGetRouteBody
-- | Automatically derived
instance ToJSON TokenGetRouteBody
  
-- | The @whoami@ route response
data WhoAmIResponse = WhoAmIResponse
                    { id :: UUID
                    -- ^ The Renaissance project uses the
                    -- <https://tools.ietf.org/html/rfc4122#appendix-B UUID>
                    -- for identifying all the resources.
                    }
  deriving (Generic)

-- | Automatically derived
instance FromJSON WhoAmIResponse
-- | Automatically derived
instance ToJSON WhoAmIResponse
