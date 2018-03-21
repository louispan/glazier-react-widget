{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Glazier.React.Widget.Action.KeyDownKey where

import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Event.Keyboard
import Glazier.React.Event.Synthetic

data KeyDownKey = KeyDownKey EventTarget J.JSString
    deriving (G.Generic, NFData)

fireKeyDownKey :: Notice -> MaybeT IO KeyDownKey
fireKeyDownKey ntc = do
    kevt <- MaybeT $ pure $ toKeyboardEvent ntc
    let evt = toSyntheticEvent ntc
        k = key kevt
    t <- lift $ pure $ target evt
    pure $ KeyDownKey t k
