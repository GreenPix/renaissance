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

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

import Component.Base
import Component.LogIn as LogIn
import Component.Nahand as Nahand

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION | eff) Unit
main = do
  let component = T.createClass (Nahand.spec settings) Nahand.initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document))
  void $ RDOM.render (R.createFactory component {}) container
