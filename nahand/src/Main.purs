module Main where

import Prelude (show, bind, ($), Unit, flip)

import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Aff.Class
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..), postAccountsNew)

type Settings = SPSettings_ SPParams_

type APIEffect eff = ReaderT Settings (ExceptT AjaxError (Aff (ajax :: AJAX | eff)))

runEffect :: forall a eff. Settings -> APIEffect eff a -> Aff (ajax :: AJAX | eff) (Either AjaxError a)
runEffect settings api = runExceptT $ runReaderT api settings

log' :: forall eff m. (MonadEff (console :: CONSOLE | eff) m) => String -> m Unit
log' x = liftEff $ log x

main :: forall eff. Eff (console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION | eff) Unit
main = do
  let settings = defaultSettings $ SPParams_ { baseURL : "http://localhost:8080/" }

  launchAff $ do
    id <- runEffect settings postAccountsNew

    case id of Right id -> log' $ show id
               Left err -> log' ":("

  log "bye"
