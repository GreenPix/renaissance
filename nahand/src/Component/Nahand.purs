module Component.Nahand where

import Prelude (($), void, bind, (>>=), show, pure, id)

import Control.Monad.Aff (later')
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
import Data.Lens (LensP, PrismP, over, lens, prism', (^.), (.~), set)
import Servant.Auth.Token.Api (PostTokenRefreshReq(..))
import GenBzApi (postTokenRefresh, getWhoami) as Bz

import Renaissance.Api.Bz.Data (WhoAmIResponse(..))
import Auth.Token (AccessGrant(..), EphemeralToken(..))

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

    -- setLogInState (LogIn _) (LogIn.Connected ag) = Connected ag "" -- connection succeed
    setLogInState (LogIn _) state = LogIn state -- the state has been update
    setLogInState x _ = x

data State = Home | LogIn LogIn.State | Connected AccessGrant String | Error String

updateAccessGrant :: AccessGrant -> State -> State
updateAccessGrant ag (Connected _ s) = Connected ag s
updateAccessGrant _ s = s

updateId :: String -> State -> State
updateId st (Connected ag _) = Connected ag st
updateId _ x = x

getAccess :: AccessGrant -> String
getAccess (AccessGrant { access : EphemeralToken et }) = et.value

getRefresh :: AccessGrant -> String
getRefresh (AccessGrant { refresh : EphemeralToken et }) = et.value

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
render dispatch _ (Connected _ h) _ = [R.p' [ R.text $ "hi :)" `append` h ] ]
render dispatch _ (Error h) _ = [R.p' [ R.text h ] ]

foreverRefreshTokens st ag = do
  let body = PostTokenRefreshReq { refreshToken : getRefresh ag }
  res <- lift $ later' 1500 $ runBzEffect st $ Bz.postTokenRefresh body
  case res of Right ag -> do void (T.cotransform $ updateAccessGrant ag)
                             foreverRefreshTokens st ag
              Left err   -> do void (T.cotransform $ pure $ Error "error with api")

performAction :: forall eff b.
                 NahandSettings
              -> T.PerformAction (console :: CONSOLE, ajax :: AJAX | eff) State b Action
performAction _ AskLogIn _ _ = do
  void (T.cotransform $ \_ -> LogIn LogIn.initialState)

performAction st (LogInAction (LogIn.ConnectionSucceed a)) x y = do
  lift $ log' $ getAccess a
  res <- lift $ runBzEffect st $ Bz.getWhoami (getAccess a)

  case res of Right (WhoAmIResponse o) -> do void (T.cotransform $ \_ -> Connected a o.id)
                                             foreverRefreshTokens st a
              Left err   -> do void (T.cotransform $ \_ -> Error "error with api")

performAction _ _ _ _ = void (T.cotransform $ id)

spec :: forall eff b. NahandSettings
                   -> T.Spec (ajax :: AJAX, console :: CONSOLE | eff) State b Action
spec st = T.withState $ \s ->
    case s of (LogIn _) -> spec1
              _         -> T.simpleSpec (performAction st) render
  where

    spec1' = T.focus _logstate _LogInAction $ T.simpleSpec (LogIn.performAction st) LogIn.render
    performAction1' = spec1' ^. T._performAction

    performAction1 ev@(LogInAction (LogIn.ConnectionSucceed _)) = performAction st ev
    performAction1 ev = performAction1' ev

    spec1 = (T._performAction .~ performAction1) spec1'
