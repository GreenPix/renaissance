module Main where

import Prelude (show, bind, ($), Unit, flip, void, (>>=), (<<<), (<$>))

import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Aff.Class
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Nullable (toMaybe)
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..), postAccountsNew)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

data State = Connected Int | Anonymous
data Action = LogInWith Int | LogOut

render :: forall t. T.Render State t Action
render dispatch _ (Anonymous) _ =
  [ R.p' [ R.text "You are not connected"
         , R.button [ RP.onClick \_ -> dispatch $ LogInWith 0 ]
                    [ R.text "Log in" ]
         ]
  ]
render dispatch _ (Connected i) _ =
  [ R.p' [ R.text $ "You are connected as " `append` show i
         , R.button [ RP.onClick \_ -> dispatch LogOut ]
                    [ R.text "Log out" ]
         ]
  ]

performAction :: forall a b. T.PerformAction a State b Action
performAction (LogInWith i) _ _ = void (T.cotransform $ \_ -> Connected i)
performAction LogOut _ _ = void (T.cotransform $ \_ -> Anonymous)

initialState :: State
initialState = Anonymous

spec :: forall a b. T.Spec a State b Action
spec = T.simpleSpec performAction render

type Settings = SPSettings_ SPParams_

type APIEffect eff = ReaderT Settings (ExceptT AjaxError (Aff (ajax :: AJAX | eff)))

runEffect :: forall a eff. Settings -> APIEffect eff a -> Aff (ajax :: AJAX | eff) (Either AjaxError a)
runEffect settings api = runExceptT $ runReaderT api settings

log' :: forall eff m. (MonadEff (console :: CONSOLE | eff) m) => String -> m Unit
log' x = liftEff $ log x

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION | eff) Unit
main = do
  let settings = defaultSettings $ SPParams_ { baseURL : "http://localhost:8080/" }

  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component {}) container

  log "done"

  -- launchAff $ do
  --   id <- runEffect settings postAccountsNew

  --   case id of Right id -> log' $ show id
  --              Left err -> log' ":("

  -- log "bye"
