{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Glazier.React.Framework.Action.KeyDownKey where

import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified Glazier.React as Z

data KeyDownKey = KeyDownKey Z.EventTarget J.JSString
    deriving (G.Generic, NFData)

fireKeyDownKey :: Z.SyntheticEvent -> MaybeT IO KeyDownKey
fireKeyDownKey evt = do
    kevt <- MaybeT $ pure $ Z.toKeyboardEvent evt
    let evt' = Z.toEvent evt
        k = Z.key kevt
    target <- lift $ pure . Z.target $ evt'
    pure $ KeyDownKey target k
