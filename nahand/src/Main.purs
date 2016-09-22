module Main where

import Prelude (bind, ($), Unit, void, (>>=), (<<<), (<$>))

import Control.Monad.Eff         (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Halogen                   (runUI, parentState)
import Halogen.Util              (runHalogenAff, awaitBody)
import Nahand.Halo.Master        (master, initialState, MasterEff)
import Network.HTTP.Affjax       (AJAX)
import Nahand.Focus              (FOCUS)

-- to be used
import Servant.PureScript.Settings (SPSettings_, defaultSettings)

main :: forall eff
      . Eff (MasterEff) Unit
main = do
  runHalogenAff $ do
    body <- awaitBody
    void $ runUI master (parentState initialState) body
