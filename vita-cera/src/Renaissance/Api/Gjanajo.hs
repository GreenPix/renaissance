{-|
Module: Renaissance.Api.Gjanajo
Copyright: (c) GreenPix, 2016
License: MIT
Stability: experimental

@gjanajo@ is the resource back-end server. It abstracts to the other servers of
the Renaissance project the storing of the static resources (the current
“game project” in use) and the dynamic resources (such as the character states).
-}

{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Renaissance.Api.Gjanajo
  ( ForeignGjanajoApi
  , GjanajoApi
  , GjanajoPublicApi
  ) where

import Servant.API
import Servant.Auth.Token.Api
import Data.UUID (UUID, fromText, toText)

import qualified Renaissance.Api.Gjanajo.Data.Account as A
import qualified Renaissance.Api.Gjanajo.Data.Character as C
import Renaissance.Api.Base
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))

-- | The 'ForeignGjanajoApi' type is a “fake” API specification which does not
-- use the experimental servant authentication combinator. In doing so, it
-- can be used by a servant-foreign user to generate client functions.
type ForeignGjanajoApi = GjanajoPublicApi

-- | The 'GjanajoApi' is the specification of @gjanajo@ rest API.
type GjanajoApi = GjanajoPublicApi

-- | It provides several routes to manipulate the accounts players,
-- including a way to create, modify and delete them. For now, they are
-- unprotected, for development convenience only. This shall be change
-- before the first release.
type GjanajoPublicApi =
       "accounts" :> GetPaginated '[JSON] A.AccountInformation
  :<|> "accounts" :> Capture "account-uuid" UUID :> Get '[JSON] A.AccountInformation
  :<|> "accounts" :> Capture "account-uuid" UUID :> "delete" :> DeleteNoContent '[JSON] NoContent
  :<|> "accounts" :> "new" :> PostCreated '[JSON] UUID


instance ToHttpApiData UUID where
    toQueryParam = toText
instance FromHttpApiData UUID where
    parseQueryParam txt = do txt <- parseUrlPiece txt
                             case fromText txt of Just uuid -> Right uuid
                                                  _         -> Left txt
