module Main where

import Prelude (show, bind, ($), Unit, flip, void, (>>=), (<<<), (<$>))

import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Trans (lift)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Monoid (append)
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

import Component.Base
import Component.LogIn as LogIn

data State = Connected Int | Anonymous | Waiting
data Action = LogInWith Int | LogOut

render :: forall t. T.Render State t Action
render dispatch _ Anonymous _ =
  [ R.p' [ R.text "You are not connected"
         , R.button [ RP.onClick \_ -> dispatch $ LogInWith 1 ]
                    [ R.text "Log in" ]
         ]
  ]
render dispatch _ (Connected i) _ =
  [ R.p' [ R.text $ "You are connected as " `append` show i
         , R.button [ RP.onClick \_ -> dispatch LogOut ]
                    [ R.text "Log out" ]
         ]
  ]
render dispatch _ Waiting _ =
  [ R.p' [ R.text $ "Waiting for server response"
         ]
  ]

performAction :: forall eff b. T.PerformAction (console :: CONSOLE, ajax :: AJAX | eff) State b Action
performAction (LogInWith i) _ _ = do
  void (T.cotransform $ \_ -> Waiting)
  res <- lift $ runBzEffect settings postAccountsNew
  case res of Right id -> do lift $ log' $ "logged as " `append` show id
                             void (T.cotransform $ \_ -> Connected id)
              Left _   -> do lift $ log' "..."
                             void (T.cotransform $ \_ -> Connected 12)

performAction LogOut _ _ = void (T.cotransform $ \_ -> Anonymous)

initialState :: State
initialState = Anonymous

spec :: forall eff b. T.Spec (ajax :: AJAX, console :: CONSOLE | eff) State b Action
spec = T.simpleSpec performAction render

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION | eff) Unit
main = do
  let component = T.createClass (LogIn.spec settings) LogIn.initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document))
  void $ RDOM.render (R.createFactory component {}) container
