{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Renaissance.Api.Gjanajo
  ( ForeignGjanajoApi
  , GjanajoApi
  , GjanajoPublicApi
  ) where

import Servant.API
import Servant.Auth.Token.Api
import Data.UUID

import qualified Renaissance.Api.Gjanajo.Data.Account as A
import qualified Renaissance.Api.Gjanajo.Data.Character as C
import Renaissance.Api.Base

type ForeignGjanajoApi = GjanajoPublicApi
type GjanajoApi = GjanajoPublicApi

type GjanajoPublicApi =
       "accounts" :> QueryFlag "stats" :> GetPaginated '[JSON] A.AccountInformation
  :<|> "accounts" :> Capture "account-uuid" UUID :> QueryFlag "stats" :> Get '[JSON] A.AccountInformation
  :<|> "accounts" :> Capture "account-uuid" UUID :> "delete" :> DeleteNoContent '[JSON] NoContent
  :<|> "accounts" :> "new" :> PostCreated '[JSON] UUID
