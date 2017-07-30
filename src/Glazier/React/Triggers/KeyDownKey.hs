module Glazier.React.Triggers.KeyDownKey where

import Control.Monad.Trans.Maybe
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

newtype KeyDownKey = KeyDownKey J.JSString

keyDownKey :: J.JSVal -> MaybeT IO KeyDownKey
keyDownKey evt = do
    syn <- MaybeT $ pure $ JE.fromJS evt
    kevt <- MaybeT $ pure $ R.parseKeyboardEvent syn
    pure . KeyDownKey $ R.key kevt
