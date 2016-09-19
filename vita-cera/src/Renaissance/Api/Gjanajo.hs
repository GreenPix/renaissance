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

type ForeignGjanajoApi = GjanajoPublicApi
type GjanajoApi = GjanajoPublicApi

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
