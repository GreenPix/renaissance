module Nahand.Halo.Master
  ( State
  , Query
  , master
  , initialState
  , LogInSlot
  , MasterEff
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Monoid (append)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct  (Coproduct)
import Halogen                 (query, request, fromEff, ParentHTML, ParentDSL, ChildF(..), ParentState, Component, parentComponent, modify)
import Halogen.HTML.Indexed    (h1_, text, div_, slot)
import Nahand.Halo.LogIn       (LoginEff, login)
import Nahand.Halo.LogIn       (State, Query(..), initialState) as LogIn

type State = { connected :: Boolean
             }

data LogInSlot = LogInSlot
derive instance eqUnit :: Eq LogInSlot
derive instance ordUnit :: Ord LogInSlot

type MasterEff = LoginEff (console :: CONSOLE)

type State' = ParentState State LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot
type Query' = Coproduct Query (ChildF LogInSlot LogIn.Query)

initialState :: State
initialState = { connected : false }

data Query a =
    Connection a
  | GetConnected (Boolean -> a)

master :: Component State' Query' (Aff MasterEff)
master = parentComponent { render, eval, peek: Just peek }
  where
    render :: State
            -> ParentHTML LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot
    render _ = div_ [ h1_ [ text "Hi" ]
                    , slot LogInSlot (\_ -> { component: login
                            , initialState: LogIn.initialState
                            })
                    ]
    
    eval :: Query
         ~> ParentDSL State LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot
    eval (Connection next) = do
      modify $ \s -> { connected: true }
      pure next
    eval (GetConnected continue) = do
      pure (continue true)

    peek :: forall x
          . ChildF LogInSlot LogIn.Query x
          -> ParentDSL State LogIn.State Query LogIn.Query (Aff MasterEff) LogInSlot Unit
    peek (ChildF p q) =
      case q of
           (LogIn.Submitting _) -> do email <- query p (request LogIn.GetEmail)
                                      case email of (Just e) -> fromEff $ log $ "hi " `append` e
                                                    _  -> pure unit
           _ -> pure unit
