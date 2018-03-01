{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Glazier.React.Framework.Action.KeyDownKey where

import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified Glazier.React as R

data KeyDownKey = KeyDownKey R.EventTarget J.JSString
    deriving (G.Generic, NFData)

fireKeyDownKey :: R.SyntheticEvent -> MaybeT IO KeyDownKey
fireKeyDownKey evt = do
    kevt <- MaybeT $ pure $ R.toKeyboardEvent evt
    let evt' = R.toEvent evt
        k = R.key kevt
    target <- lift $ pure . R.target $ evt'
    pure $ KeyDownKey target k
