{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified Data.JSString as J
import Data.Maybe
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Actions as A
import qualified JavaScript.Extras as JE

data SubmitInput = SubmitInput R.EventTarget J.JSString
data CancelInput = CancelInput R.EventTarget

inputPrototype
    :: (Which '[SubmitInput, CancelInput] -> F.Delegate specs)
    -> F.Prototype '[] reqs '[] specs
inputPrototype hdl = F.Prototype ( F.idle
                             , F.display disp
                             , D.singleton ("onKeyDown", \j e -> go j (`hdl` e))
                             )
  where
    disp ls ps dsn = R.lf "input" (ls dsn) (ps dsn)
    go j onAction = fmap (fromMaybe ()) . runMaybeT $ do
        A.KeyDownKey target k <- A.fireKeyDownKey j
        case k of
            "Escape" -> lift . atomically . onAction . pick $ CancelInput target
            "Enter" -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
                lift . atomically . onAction . pick $ SubmitInput target v
            _ -> empty
