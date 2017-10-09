{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input.TextInput
    ( textInputPrototype
    ) where

import Data.Diverse
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Widgets.Input as W

textInputPrototype
    :: (R.MonadReactor m, UniqueMember W.SubmitInput ts, UniqueMember W.CancelInput ts)
    => F.Prototype m '[] reqs '[] specs '[] hs '[W.SubmitInput, W.CancelInput] ts '[] as '[] hcs acs
textInputPrototype =
    W.inputPrototype `F.andPrototype` F.displaying (F.decorate [("type", "text")])
