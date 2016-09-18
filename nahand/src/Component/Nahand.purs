module Component.Nahand where

import Prelude (($), void, bind, (>>=), show, pure, id)

import Data.Monoid (append)
import Data.Maybe (Maybe(..))
import Thermite as T
import React as R
import Data.Either (Either(..))
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Unsafe.Coerce (unsafeCoerce) -- TODO: find a way to remove it
import Control.Monad.Eff.Console (log, CONSOLE)
import Network.HTTP.Affjax (AJAX)
import Renaissance.Api.Bz.Data (TokenGetRouteBody(..))
import Control.Monad.Trans (lift)
import Servant.PureScript.Affjax (errorToString)
import Data.Lens (LensP, PrismP, over, lens, prism')

import Auth.Token (AccessGrant)

import Component.Base
import Component.LogIn as LogIn

-- | When using focus, the inner container event is first risen to the parent,
--   using the Prism. Then, once proccessed, it is sent back to the original
--   container. A prism is a way to make the container communicates.
--
--   Note that a parent has to process all the events of its children, but the
--   contrary is false.
_LogInAction :: PrismP Action (LogIn.Action)
_LogInAction = prism' LogInAction actionToLogIn
  where
    actionToLogIn (LogInAction act) = Just act
    actionToLogIn _ = Nothing

-- | A lens is basically a getter and a setter. The state of the children is
--   retreive from the state of the parent and, then, the parent is updated
--   with the new value of its child.
_logstate :: LensP State LogIn.State
_logstate = lens getLogInState setLogInState
  where
    getLogInState (LogIn state) = state
    getLogInState _ = LogIn.initialState

    setLogInState (LogIn _) state = LogIn state -- the state has been update
    setLogInState _ (LogIn.Connected ag) = Connected ag -- connection succeed
    setLogInState x _ = x

data State = Home | LogIn LogIn.State | Connected AccessGrant

initialState :: State
initialState = Home

data Action = AskLogIn | LogInAction LogIn.Action

render :: forall props. T.Render State props Action
render dispatch _ Home _ =
  [ R.h1' [ R.text "Welcome to Renaissance"
          ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch $ AskLogIn ]
                    [ R.text "Log in" ]
         ]
  , R.p' [ R.button []
                    [ R.text "Create an account" ]
         ]
  ]
render dispatch _ (LogIn _) _ = []
render dispatch _ (Connected _) _ = [R.p' [ R.text "hi :)"
                                          ]
                                    ]

performAction :: forall eff b.
                 NahandSettings
              -> T.PerformAction (console :: CONSOLE, ajax :: AJAX | eff) State b Action
performAction _ AskLogIn _ _ = do
  void (T.cotransform $ \_ -> LogIn LogIn.initialState)
-- TODO: understand why this line does nothing and the LensP setter does the job
performAction _ (LogInAction (LogIn.ConnectionSucceed ag)) _ _ = void (T.cotransform $ \st -> Connected ag)
performAction _ _ _ _ = void (T.cotransform $ id)

spec :: forall eff b. NahandSettings
                   -> T.Spec (ajax :: AJAX, console :: CONSOLE | eff) State b Action
spec st = T.withState $ \s ->
    case s of (LogIn _) -> T.focus _logstate _LogInAction $ T.simpleSpec (LogIn.performAction st) LogIn.render
              _         -> T.simpleSpec (performAction st) render
