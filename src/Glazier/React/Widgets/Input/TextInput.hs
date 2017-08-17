{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input.TextInput
    ( textInputPrototype
    ) where

import Control.Concurrent.STM
import Data.Diverse.Lens
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Widgets.Input as W

textInputPrototype
    :: (TMVar (F.Design specs) -> Which '[W.SubmitInput, W.CancelInput] -> STM ())
    -> F.Prototype '[] reqs '[] specs
textInputPrototype hdl =
    W.inputPrototype hdl `F.andPrototype` F.displaying (F.decorate [("type", "text")])
