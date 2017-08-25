{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input.TextInput
    ( textInputPrototype
    ) where

import Data.Diverse
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Widgets.Input as W

textInputPrototype
    :: (UniqueMember W.SubmitInput acts, UniqueMember W.CancelInput acts)
    => F.Prototype '[] reqs '[] specs '[W.SubmitInput, W.CancelInput] acts
textInputPrototype =
    W.inputPrototype `F.andPrototype` F.displaying (F.decorate [("type", "text")])
