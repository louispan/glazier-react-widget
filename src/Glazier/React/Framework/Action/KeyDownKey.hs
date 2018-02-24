{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Glazier.React.Framework.Action.KeyDownKey where

import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

data KeyDownKey = KeyDownKey R.EventTarget J.JSString
    deriving (G.Generic, NFData)

fireKeyDownKey :: JE.JSRep -> MaybeT IO KeyDownKey
fireKeyDownKey evt = do
    syn <- MaybeT $ pure $ JE.fromJSR evt
    kevt <- MaybeT $ pure $ R.parseKeyboardEvent syn
    let evt' = R.parseEvent syn
        k = R.key kevt
    target <- lift $ pure . R.target $ evt'
    pure $ KeyDownKey target k
