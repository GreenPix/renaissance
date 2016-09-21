module Nahand.Halo.Master
  ( State
  , Query
  , master
  ) where

import Prelude
import Halogen (Component, component, ComponentHTML, ComponentDSL, modify)
import Halogen.HTML.Indexed (h1_, text)

type State = { connected :: Boolean
             }

initialState :: State
initialState = { connected : true }

data Query a =
    Connection a
  | GetConnected (Boolean -> a)

master :: forall g
        . Component State Query g
master = component { render, eval }
  where
    render :: State
            -> ComponentHTML Query
    render _ = h1_ [ text "Hi" ]
    
    eval :: Query
         ~> ComponentDSL State Query g
    eval (Connection next) = do
      modify $ \s -> { connected: true }
      pure next
    eval (GetConnected continue) = do
      pure (continue true)
    
