module Nahand.Halo.ChooseCharacter
  ( ChooseCharacterEff
  , chooseCharacter
  , State
  , initialState
  , Query(..)
  ) where

import Control.Monad.Aff                (Aff)
import Data.Maybe                       (Maybe(..))
import Halogen                          (modify, Component, ComponentHTML, ComponentDSL)
import Halogen.Component                (lifecycleComponent)
import Halogen.HTML.Indexed             (text, div_) as H
import Halogen.Query                    (action)
import Prelude

type ChooseCharacterEff eff = Aff eff

data State = State { account     :: Maybe String
                   }
setAccount :: String -> State -> State
setAccount st (State o) = State $ o { account = Just st }

initialState :: State
initialState = State { account : Nothing }

data Query a = RequestAccount a
             | SetAccount String a

chooseCharacter :: forall eff
                 . Component State Query (ChooseCharacterEff eff)
chooseCharacter = lifecycleComponent { render:  render
                                     , eval:     eval
                                     , initializer: Just (action RequestAccount)
                                     , finalizer: Nothing }
  where
    render :: State
            -> ComponentHTML Query
    render s@(State o) =
      H.div_ [ H.text case o.account of Just n  -> "Welcome back, " `append` n
                                        Nothing -> "Waiting for server response"
             ]
    
    eval :: Query
         ~> ComponentDSL State Query (ChooseCharacterEff eff)
    eval (RequestAccount next) = do pure next
    eval (SetAccount st next) = do modify $ setAccount st
                                   pure next
