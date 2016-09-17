module Component.Base where

import Prelude (show, bind, ($), Unit, flip, void, (>>=), (<<<), (<$>))

import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Servant.PureScript.Settings (SPSettings_(..), defaultSettings)
import ServerAPI (SPParams_) as Bz
import Data.Either (Either(..))
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Servant.PureScript.Affjax (AjaxError)
import Network.HTTP.Affjax (AJAX)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Generic (gShow)

import ServerAPI (SPParams_(..))

type BzSettings = SPSettings_ Bz.SPParams_

data NahandSettings = NahandSettings { bzSettings :: BzSettings
                                     }
runEffect :: forall a eff s.
             s
          -> APIEffect eff s a
          -> Aff (ajax :: AJAX | eff) (Either AjaxError a)
runEffect st api = runExceptT $ runReaderT api st

runBzEffect :: forall a eff.
               NahandSettings
            -> BzEffect eff a
            -> Aff (ajax :: AJAX | eff) (Either AjaxError a)
runBzEffect (NahandSettings st) = runEffect (st.bzSettings)

log' :: forall eff m.
        (MonadEff (console :: CONSOLE | eff) m)
     => String
     -> m Unit
log' x = liftEff $ log x

settings :: NahandSettings
settings = NahandSettings { bzSettings : SPSettings_ { decodeJson : decodeJson
                                                     , encodeJson : encodeJson
                                                     , toURLPiece : gShow
                                                     , params : SPParams_ { baseURL : "http://localhost:8080/" }
                                                     }
                          }

type APIEffect eff s = ReaderT s (ExceptT AjaxError (Aff (ajax :: AJAX | eff)))
type BzEffect eff = ReaderT BzSettings (ExceptT AjaxError (Aff (ajax :: AJAX | eff)))
