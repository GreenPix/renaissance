module Nahand.Halo.Master
  ( State
  , Query
  , master
  , initialState
  , LogInSlot
  , MasterEff
  ) where

import Servant.Auth.Token.Api (PostTokenRefreshReq(..))
import Auth.Token (AccessGrant(..), EphemeralToken(..))
import Prelude
import Control.Monad.Aff (Aff, later')
import Network.HTTP.Affjax (AJAX)
import Data.Maybe (Maybe(..), isNothing)
import Data.Either (Either(..))
import Data.Monoid (append)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Functor.Coproduct  (Coproduct)
import Halogen                 (query, action, request, fromAff, ParentHTML, ParentDSL, ChildF(..), ParentState, Component, parentComponent, modify)
import Halogen.HTML.Indexed    (h1_, text, div_, slot)
import Nahand.Halo.LogIn       (LoginEff, login)
import Nahand.Halo.LogIn       (State, Query(..), initialState) as LogIn
import Nahand.Halo.Base        (runBzEffect, settings)

import Renaissance.Api.Bz.Data (TokenGetRouteBody(..))
import GenBzApi (postTokenGet, postTokenRefresh)

type State = { connected :: Maybe AccessGrant
             , errorMsg :: Maybe String
             }

data LogInSlot = LogInSlot
derive instance eqUnit :: Eq LogInSlot
derive instance ordUnit :: Ord LogInSlot

type MasterEff = LoginEff (console :: CONSOLE, ajax :: AJAX)

type State' = ParentState State LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot
type Query' = Coproduct Query (ChildF LogInSlot LogIn.Query)

initialState :: State
initialState = { connected : Nothing
               , errorMsg : Nothing }

connectionSucceed :: AccessGrant -> State -> State
connectionSucceed ag o = o { connected = Just ag }

errorMsg :: String -> State -> State
errorMsg st o = o { errorMsg = Just st }

data Query a =
  Sleep a

master :: Component State' Query' (Aff MasterEff)
master = parentComponent { render, eval, peek: Just peek }
  where
    render :: State
            -> ParentHTML LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot
    render o = if isNothing o.connected
               then div_ [ h1_ [ text "Hi" ]
                         , slot LogInSlot (\_ -> { component: login
                                                 , initialState: LogIn.initialState
                                                 })
                         ]
                else div_ [ text "connected" ]
    
    eval :: Query
         ~> ParentDSL State LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot
    eval (Sleep next) = pure next

    peek :: forall x
          . ChildF LogInSlot LogIn.Query x
          -> ParentDSL State LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot Unit
    peek (ChildF p q) =
      case q of
           (LogIn.Submitting _) ->
               do email <- query p (request LogIn.GetEmail)
                  res <- askTokenForEmail email
                  case res of
                       (Right (Right ag)) -> do modify $ connectionSucceed ag
                                                waitAndRefresh ag
                       _ -> modify $ errorMsg "fail to connect"
           _ ->
               pure unit

    askTokenForEmail (Just email) =
      do let body = TokenGetRouteBody { email: email }
         res <- fromAff $ runBzEffect settings (postTokenGet body)
         pure $ Right res
    askTokenForEmail Nothing = pure $ Left "No email to use"

    waitAndRefresh ag =
      do let body = PostTokenRefreshReq { refreshToken : unwrapRefreshToken ag }
         res <- fromAff $ later' 2000 $ runBzEffect settings (postTokenRefresh body)
         case res of Right ag' -> do modify $ connectionSucceed ag'
                                     waitAndRefresh ag'
                     _         -> do modify $ errorMsg "unable to refresh"

unwrapRefreshToken :: AccessGrant -> String
unwrapRefreshToken (AccessGrant o) = case o.refresh of (EphemeralToken o) -> o.value
