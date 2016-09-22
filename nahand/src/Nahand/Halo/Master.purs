module Nahand.Halo.Master
  ( State
  , Query
  , master
  , initialState
  , LogInSlot
  , CCSlot
  , MasterEff
  ) where

import Prelude

import Servant.Auth.Token.Api    (PostTokenRefreshReq(..))
import Auth.Token                (AccessGrant(..), EphemeralToken(..))
import Control.Monad.Aff         (Aff, later')
import Network.HTTP.Affjax       (AJAX)
import Data.Maybe                (Maybe(..), isNothing)
import Data.Either               (Either(..))
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor.Coproduct    (Coproduct, unCoproduct)
import Halogen                   (get, query, query', fromEff, action, request, fromAff, ParentHTML, ParentDSL, ChildF(..), ParentState, Component, parentComponent, modify)
import Halogen.HTML.Indexed      (h1_, text, div_, slot')
import Nahand.Halo.LogIn         (LoginEff, login)
import Nahand.Halo.LogIn         (State, Query(..), initialState) as LogIn
import Nahand.Halo.Base          (runBzEffect, settings)
import Nahand.Halo.ChooseCharacter (ChooseCharacterEff, chooseCharacter)
import Nahand.Halo.ChooseCharacter (State, Query(..), initialState) as CC
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR)

import Renaissance.Api.Bz.Data (TokenGetRouteBody(..), WhoAmIResponse(..))
import GenBzApi (postTokenGet, postTokenRefresh, getWhoami)

type State = { connected :: Maybe AccessGrant
             , errorMsg :: Maybe String
             }

data LogInSlot = LogInSlot
derive instance eqLogIn :: Eq LogInSlot
derive instance ordLogIn :: Ord LogInSlot

data CCSlot = CCSlot
derive instance eqCC :: Eq CCSlot
derive instance ordCC :: Ord CCSlot

type ChildState = Either LogIn.State CC.State
type ChildQuery = Coproduct LogIn.Query CC.Query
type ChildSlot = Either LogInSlot CCSlot

cpLogIn :: ChildPath LogIn.State ChildState LogIn.Query ChildQuery LogInSlot ChildSlot
cpLogIn = cpL

cpCC :: ChildPath CC.State ChildState CC.Query ChildQuery CCSlot ChildSlot
cpCC = cpR

type MasterEff = LoginEff (console :: CONSOLE, ajax :: AJAX)
type MasterHTML = ParentHTML ChildState Query ChildQuery (Aff MasterEff) ChildSlot
type MasterDSL = ParentDSL State ChildState Query ChildQuery (Aff MasterEff) ChildSlot

type State' = ParentState State ChildState Query ChildQuery (Aff MasterEff) ChildSlot
type Query' = Coproduct Query (ChildF ChildSlot ChildQuery)

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
            -> MasterHTML
    render o = div_ if isNothing o.connected
                    then [ h1_ [ text "Hi" ]
                              , slot' cpLogIn LogInSlot (\_ -> { component: login
                                                               , initialState: LogIn.initialState
                                                               })
                              ]
                    else [ slot' cpCC CCSlot (\_ -> { component: chooseCharacter
                                                     , initialState: CC.initialState
                                                          }) ]
    
    eval :: Query
         ~> MasterDSL
    eval (Sleep next) = pure next

    peek :: forall x
          . ChildF ChildSlot ChildQuery x
          -> MasterDSL Unit
    peek (ChildF _ ev) = case unCoproduct ev of
                                 Right ev -> peekCC ev
                                 Left ev -> peekLogIn ev

    peekLogIn :: forall a
               . LogIn.Query a
               -> MasterDSL Unit
    peekLogIn (LogIn.Submitting _) =
        do email <- query' cpLogIn LogInSlot (request LogIn.GetEmail)
           askTokenForEmail email
    peekLogIn _ = pure unit

    peekCC :: forall a
            . CC.Query a
            -> MasterDSL Unit
    peekCC (CC.RequestAccount _) =
      do tok <- getAccessToken
         case tok of Just t -> do fromEff $ log t
                                  whoAmI t
                     Nothing -> pure unit
    peekCC _ = pure unit


askTokenForEmail :: Maybe String -> MasterDSL Unit
askTokenForEmail (Just email) =
  do let body = TokenGetRouteBody { email: email }
     res <- fromAff $ runBzEffect settings (postTokenGet body)
     case res of (Right ag) -> do modify $ connectionSucceed ag
                                  waitAndRefresh ag
                 _ -> modify $ errorMsg "fail to connect"

askTokenForEmail Nothing = modify $ errorMsg "invalid state"

whoAmI :: String -> MasterDSL Unit
whoAmI t = do res <- fromAff $ runBzEffect settings (getWhoami t)
              case res of
                   Left _ -> modify $ errorMsg "not able to ask account info"
                   Right wai -> do case wai of WhoAmIResponse o -> query' cpCC CCSlot (action $ CC.SetAccount o.id)
                                   pure unit

waitAndRefresh :: AccessGrant -> MasterDSL Unit
waitAndRefresh ag =
  do let body = PostTokenRefreshReq { refreshToken : unwrapRefreshToken ag }
     res <- fromAff $ later' 2000 $ runBzEffect settings (postTokenRefresh body)
     case res of Right ag' -> do modify $ connectionSucceed ag'
                                 waitAndRefresh ag'
                 _         -> do modify $ errorMsg "unable to refresh"

getAccessToken :: MasterDSL (Maybe String)
getAccessToken = do s <- get
                    pure $ (do ag <- s.connected
                               pure $ unwrapAccessToken ag)

unwrapAccessToken :: AccessGrant -> String
unwrapAccessToken (AccessGrant o) = case o.access of (EphemeralToken o) -> o.value

unwrapRefreshToken :: AccessGrant -> String
unwrapRefreshToken (AccessGrant o) = case o.refresh of (EphemeralToken o) -> o.value
