module Component.LogIn where

import Prelude (($), void, bind, (>>=), show)

import Data.Monoid (append)
import Thermite as T
import React as R
import Data.Either (Either(..))
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Unsafe.Coerce (unsafeCoerce) -- TODO: find a way to remove it
import Control.Monad.Eff.Console (log, CONSOLE)
import Network.HTTP.Affjax (AJAX)
import ServerAPI (postAccountsNew, postTokenGet)
import Renaissance.Api.Bz.Data (TokenGetRouteBody(..))
import Control.Monad.Trans (lift)
import Servant.PureScript.Affjax (errorToString)

import Component.Base

import Auth.Token (AccessGrant)

data State = Form String | Waiting | Connected AccessGrant | Error

initialState = Form ""

data Action = Change String | LogAs String

render :: forall props. T.Render State props Action
render dispatch _ (Form st) _ =
  [ R.p' [ R.text "Give your email"
         , R.input [ RP.value st
                   , RP.onChange \e -> dispatch (Change (unsafeCoerce e).target.value) ]
                   []
         , R.button [ RP.onClick \e -> dispatch $ LogAs (unsafeCoerce e).target.value ]
                    [ R.text "Log in" ]
         ]
  ]
render dispatch _ Waiting _ =
  [ R.p' [ R.text "Waiting for server response"
         ]
  ]
render dispatch _ Error _ =
  [ R.p' [ R.text "Error with the server"
         ]
  ]
render dispatch _ _ _ = []

performAction :: forall eff b.
                 NahandSettings
              -> T.PerformAction (console :: CONSOLE, ajax :: AJAX | eff) State b Action
performAction _ (Change st) _ _ = do
  void (T.cotransform $ \_ -> Form st)
performAction s (LogAs st) _ _ = do
  void (T.cotransform $ \_ -> Waiting)

  let body = TokenGetRouteBody { email : st }

  res <- lift $ runBzEffect s $ postTokenGet body
  case res of Right ag -> do void (T.cotransform $ \_ -> Connected ag)
              Left err   -> do lift $ log' $ errorToString err
                               void (T.cotransform $ \_ -> Error)

spec :: forall eff b. NahandSettings
                   -> T.Spec (ajax :: AJAX, console :: CONSOLE | eff) State b Action
spec st = T.simpleSpec (performAction st) render
