module Nahand.Halo.LogIn
  ( State
  , Query
  , login
  , initialState
  , loginEff
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Apply ((*>))
import Data.Functor (($>))
import Data.Eq
import Halogen.Query                    (action)
import Halogen                          (fromEff, get, Component, component, ComponentHTML, ComponentDSL, modify)
import Halogen.Component                (lifecycleComponent)
import Halogen.HTML.Core                (HTML, className)
import Halogen.HTML.Indexed             (h1_, text, div_, div, button, input) as H
import Halogen.HTML.Events.Indexed      (onKeyDown, onValueInput, onClick, input_, input) as E
import Halogen.HTML.Events.Types        (KeyboardEvent) as E
import Halogen.HTML.Properties.Indexed  (tabIndex, id_, autofocus, value, disabled, class_) as P
import Halogen.HTML.Events.Handler      (stopPropagation, preventDefault) as Ha
import Data.Maybe                       (Maybe(..))
import Nahand.Focus                     (FOCUS, setFocus)

arrowUP :: Number
arrowUP = 38.0
arrowDOWN :: Number
arrowDOWN = 40.0

type LoginEff eff = Aff (focus :: FOCUS | eff)

data Focus = Form | Button
derive instance eqFocus :: Eq Focus

data State = State { emailValue :: String
                   , focus      :: Focus
                   }

setEmailValue :: String -> State -> State
setEmailValue st (State o) = State $ o { emailValue = st }

changeFocus :: State -> State
changeFocus (State o) = if o.focus == Form
                        then State $ o { focus = Button }
                        else State $ o { focus = Form }

emailInput :: forall p. State -> HTML p (Query Unit)
emailInput (State o) =
  H.div_ [ H.input [ E.onValueInput (E.input UpdateEmail)
                   , P.value o.emailValue
                   , P.id_ "login_input"
                   , P.disabled $ not $ o.focus == Form
                   , P.autofocus $ o.focus == Form
                   , P.class_ $ inputClass o.focus
                   ]
         ]
    where
      inputClass Form = className "focus"
      inputClass _    = className "no-focus"

submitButton :: forall p. State -> HTML p (Query Unit)
submitButton (State o) =
  H.div_ [ H.button [ E.onClick (E.input_ Submitting)
                    , P.disabled $ not $ o.focus == Button
                    , P.id_ "login_button"
                    , P.class_ $ inputClass o.focus
                    ]
                    [ H.text "submit" ]
         ]
    where
      inputClass Button = className "focus"
      inputClass _      = className "no-focus"

initialState :: State
initialState = State { emailValue: ""
                     , focus:      Form
                     }

data Query a = FocusChange a
             | StartTyping a
             | UpdateEmail String a
             | Submitting a
             | Init a
             | GetEmail (String -> a)

login :: forall eff
       . Component State Query (LoginEff eff)
login = lifecycleComponent { render:  render
                           , eval:     eval
                           , initializer: Just (action Init)
                           , finalizer: Nothing }
  where
    render :: State
            -> ComponentHTML Query
    render s@(State o) =
      H.div [ E.onKeyDown dealKey
            , P.tabIndex 0
            ]
            [ H.h1_ [ H.text "Try login" ]
            , emailInput s
            , submitButton s
            ]
       where
         dealKey o =
          if o.keyCode == arrowUP || o.keyCode == arrowDOWN
          then Ha.preventDefault *> Ha.stopPropagation $> Just (action FocusChange)
          else pure Nothing

    
    eval :: Query
         ~> ComponentDSL State Query (LoginEff eff)
    eval (Init next) = do fromEff $ setFocus "login_input"
                          pure next
                          
    eval (FocusChange next) = do modify changeFocus
                                 s <- get
                                 updateFocus s
                                 pure next

    eval (StartTyping next) = pure next

    eval (UpdateEmail st next) = do modify $ setEmailValue st
                                    pure next

    eval (Submitting next) = pure next

    eval (GetEmail continue) = do s <- get
                                  case s of State o -> pure (continue o.emailValue)

    updateFocus (State o) = fromEff if o.focus == Form
                                    then setFocus "login_input"
                                    else setFocus "login_button"
