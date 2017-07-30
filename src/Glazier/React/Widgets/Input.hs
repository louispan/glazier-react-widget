{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input
    ( InputAction(..)
    , inputPrototype
    ) where

import Control.Monad.Reader
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands.Property as C
import qualified JavaScript.Extras as JE

data InputAction
    = SubmitAction R.EventTarget J.JSString
    | CancelAction R.EventTarget

inputPrototype
    :: (UniqueMember InputAction acts, UniqueMember C.PropertyCommand cmds)
    => F.Prototype '[] ols '[] dtls '[] plns '[] trigs '[InputAction] acts '[C.PropertyCommand] cmds
inputPrototype =
    (F.statically $ F.display d) `F.orPrototype`
    (F.dynamically (F.gadgetry gadget))
  where
    d ls ps = lift $ R.lf "input" ls ps

gadget :: G.Gadget InputAction (F.Entity dtls plns) (D.DList C.PropertyCommand)
gadget = do
    a <- ask
    case a of
        CancelAction j -> pure $ D.singleton $ C.SetPropertyCommand (JE.toJS j) ("value", JE.toJS' J.empty)
        _ -> pure mempty
