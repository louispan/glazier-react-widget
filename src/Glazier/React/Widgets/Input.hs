{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.DList as DL
import qualified Data.JSString as J
import Data.Proxy
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Actions as A
import qualified JavaScript.Extras as JE

data SubmitInput = SubmitInput R.EventTarget J.JSString
data CancelInput = CancelInput R.EventTarget

inputPrototype
    :: (UniqueMember SubmitInput acts', UniqueMember CancelInput acts')
    => F.Prototype v '[] reqs '[] specs '[] acts '[SubmitInput, CancelInput] '[] acts' '[] '[] cmds
inputPrototype = F.Prototype ( mempty
                             , F.display disp
                             , F.Triggers (Proxy, DL.singleton ("onKeyDown", go))
                             , mempty
                             )
  where
    disp ls ps dsn = R.lf "input" (ls dsn) (ps dsn)
    go = R.handleEventM A.fireKeyDownKey goLazy
      where
        goLazy (A.KeyDownKey target k) = case k of
            "Escape" -> pure . pick $ CancelInput target
            "Enter" -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
                pure . pick $ SubmitInput target v
            _ -> empty
