{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input.TextInput
    ( textInputPrototype
    ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands.Property as C
import qualified Glazier.React.Widgets.Input as W
import qualified Glazier.React.Triggers.KeyDownKey as T
import qualified JavaScript.Extras as JE

textInputPrototype
    :: ( UniqueMember W.InputAction acts
       , UniqueMember C.PropertyCommand cmds
       , UniqueMember T.KeyDownKeyTrigger trigs
       )
    => Applicative m => F.Prototype m '[] ols
                                      '[] dtls
                                      '[] plns
                                      '[T.KeyDownKeyTrigger] trigs
                                      '[W.InputAction] '[W.InputAction] acts
                                      '[C.PropertyCommand] '[] cmds
                                      '[] envs
textInputPrototype =
    W.inputPrototype
    `F.andPrototype` (F.statically $ F.hardcode [("type", "text")])
    `F.andPrototype` (F.triggering $ F.trigger "onKeyDown" T.keyDownKeyTrigger go)
  where
    go (T.KeyDownKeyTrigger target k) = case k of
        "Escape" -> pure $ W.CancelAction target
        "Enter" -> do
            v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
            pure $ W.SubmitAction target v
        _ -> empty
