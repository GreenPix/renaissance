module Main where

import Prelude (bind, ($), Unit, void, (>>=), (<<<), (<$>))

import Control.Monad.Eff         (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Halogen                   (runUI)
import Halogen.Effects           (HalogenEffects)
import Halogen.Util              (runHalogenAff, awaitBody)
import Nahand.Halo.Master        (master, initialState)
import Network.HTTP.Affjax       (AJAX)

-- to be used
import Servant.PureScript.Settings (SPSettings_, defaultSettings)

main :: forall eff
      . Eff (HalogenEffects (console :: CONSOLE, ajax :: AJAX|eff)) Unit
main = do
  runHalogenAff $ do
    body <- awaitBody
    void $ runUI master initialState body
