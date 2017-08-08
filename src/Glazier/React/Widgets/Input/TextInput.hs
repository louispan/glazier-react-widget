{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input.TextInput
    ( textInputPrototype
    ) where

import Data.Diverse.Lens
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands as C
import qualified Glazier.React.Triggers as T
import qualified Glazier.React.Widgets.Input as W

textInputPrototype
    :: ( UniqueMember T.KeyDownKeyTrigger trigs
       , UniqueMember W.InputAction acts
       , UniqueMember C.SetPropertyCommand cmds
       )
    => F.Prototype IO
                   '[] ols
                   '[] dtls
                   '[] plns
                   '[T.KeyDownKeyTrigger] trigs
                   '[W.InputAction] '[W.InputAction] acts
                   '[C.SetPropertyCommand] '[C.SetPropertyCommand] cmds
                   '[] envs
textInputPrototype =
    W.inputPrototype `F.andPrototype` (F.statically $ F.hardcode [("type", "text")])
