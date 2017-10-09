{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.JSString as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Actions as A
import qualified JavaScript.Extras as JE

data SubmitInput = SubmitInput R.EventTarget J.JSString
data CancelInput = CancelInput R.EventTarget

inputPrototype
    :: (R.MonadReactor m, UniqueMember SubmitInput ts, UniqueMember CancelInput ts)
    => F.Prototype m '[] reqs '[] specs '[] hs '[SubmitInput, CancelInput] ts '[] as '[] hcs acs
inputPrototype =
    F.Prototype
        ( F.display disp
        , F.idle
        , F.ignore
        , F.trigger "onKeyDown" (A.fireKeyDownKey >=> go)
        , F.inert)
  where
    disp ls ps dsn = R.lf "input" (ls dsn) (ps dsn)
    go (A.KeyDownKey target k) =
        case k of
            "Escape" -> pure . pick $ CancelInput target
            "Enter" -> do
                v <-
                    MaybeT $
                    JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
                pure . pick $ SubmitInput target v
            _ -> empty
