module Glazier.React.Triggers.KeyDownKey where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

data KeyDownKeyTrigger = KeyDownKeyTrigger R.EventTarget J.JSString

keyDownKeyTrigger :: J.JSVal -> MaybeT IO KeyDownKeyTrigger
keyDownKeyTrigger evt = do
    syn <- MaybeT $ pure $ JE.fromJS evt
    kevt <- MaybeT $ pure $ R.parseKeyboardEvent syn
    let evt' = R.parseEvent syn
        k = R.key kevt
    target <- lift $ pure . R.target $ evt'
    pure $ KeyDownKeyTrigger target k
