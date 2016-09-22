module Nahand.Focus where

import Prelude
import Control.Monad.Eff         (Eff)
import Data.Function.Uncurried   (runFn1, Fn1)

foreign import data FOCUS :: !

foreign import setFocusImpl :: forall e. Fn1 String (Eff (focus :: FOCUS | e) Unit)

setFocus :: forall e. String -> Eff (focus :: FOCUS | e) Unit
setFocus = runFn1 setFocusImpl
