{-|
Module: Renaissance.Api.Gjanajo.Data.Account
Copyright: (c) GreenPix, 2016
License: MIT
Stability: experimental

This module provides the several data types used by the @gjanajo@ Api when
it comes to accounts.
-}
{-# LANGUAGE DeriveGeneric #-}

module Renaissance.Api.Gjanajo.Data.Account where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import GHC.Int
import Data.Time
import Data.UUID.Aeson
import Data.UUID


-- | The results of the several routes which provides the delails of an
-- account.
data AccountInformation = AccountInformation { email    :: Text
                                             -- ^ The email used to
                                             -- authenticate
                                             , lastSeen :: UTCTime
                                             -- ^ The last time the player
                                             -- has been seen, that is it
                                             -- has logged into lycan
                                             , uuid     :: UUID
                                             -- ^ The Renaissance project
                                             -- uses the
                                             -- <https://tools.ietf.org/html/rfc4122#appendix-B UUID>
                                             -- for identifying all the
                                             -- resources.
                                             }
  deriving (Generic)

-- | Automatically derived
instance FromJSON AccountInformation
-- | Automatically derived
instance ToJSON AccountInformation
