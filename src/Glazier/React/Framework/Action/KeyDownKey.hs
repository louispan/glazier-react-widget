{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Glazier.React.Framework.Action.KeyDownKey where

import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import Glazier.React

data KeyDownKey = KeyDownKey EventTarget J.JSString
    deriving (G.Generic, NFData)

fireKeyDownKey :: SyntheticEvent -> MaybeT IO KeyDownKey
fireKeyDownKey evt = do
    kevt <- MaybeT $ pure $ toKeyboardEvent evt
    let evt' = toEvent evt
        k = key kevt
    t <- lift $ pure . target $ evt'
    pure $ KeyDownKey t k
