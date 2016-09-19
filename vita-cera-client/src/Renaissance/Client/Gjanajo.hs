module Renaissance.Client.Gjanajo where

import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy (Proxy(..))
import Data.UUID (UUID, toText)
import Network.HTTP.Client (Manager)
import Servant.API ((:<|>) ((:<|>)), NoContent)
import Servant.Client (client, BaseUrl, ServantError, AuthenticateReq)
import Servant.Common.Req (Req, addHeader)

import Renaissance.Api.Base
import Renaissance.Api.Gjanajo (GjanajoApi)
import qualified Renaissance.Api.Gjanajo.Data.Account as A (AccountInformation)
import qualified Renaissance.Api.Gjanajo.Data.Character as C (CharacterInformation, Statistics)

gjanajoApi :: Proxy GjanajoApi
gjanajoApi = Proxy

accountsGet :: Maybe Int
            -> Maybe Int
            -> Manager
            -> BaseUrl
            -> ExceptT ServantError IO [A.AccountInformation]

accountsByUuid :: UUID
               -> Manager
               -> BaseUrl
               -> ExceptT ServantError IO A.AccountInformation

accountsDelete :: UUID
               -> Manager
               -> BaseUrl
               -> ExceptT ServantError IO NoContent

accountsNewPost :: Manager
                -> BaseUrl
                -> ExceptT ServantError IO UUID

accountsGet :<|> accountsByUuid :<|> accountsDelete :<|> accountsNewPost = client gjanajoApi
