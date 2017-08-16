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
-- import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
-- import qualified Glazier.React.Commands as C
import qualified Glazier.React.Actions as A
import qualified JavaScript.Extras as JE

-- data InputAction
--     = SubmitAction R.EventTarget J.JSString
--     | CancelAction R.EventTarget

data SubmitInput = SubmitInput R.EventTarget J.JSString
data CancelInput = CancelInput R.EventTarget

inputPrototype
    :: ( UniqueMember (TMVar (F.Design specs) -> SubmitInput -> STM ()) hdls
       , UniqueMember (TMVar (F.Design specs) -> CancelInput -> STM ()) hdls
       )
    => Many hdls -> F.Prototype '[] reqs '[] specs
inputPrototype hdls = F.Prototype ( F.idle
                             , F.display disp
                             , D.singleton ("onKeyDown", \d -> go (fetch hdls d) (fetch hdls d))
                             )
  where
    disp ls ps dsn = R.lf "input" (ls dsn) (ps dsn)
    go onCancelInput onSubmitInput j = fmap (fromMaybe ()) . runMaybeT $ do
        A.KeyDownKey target k <- A.fireKeyDownKey j
        case k of
            "Escape" -> lift . atomically . onCancelInput $ CancelInput target
            "Enter" -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
                lift . atomically . onSubmitInput $ SubmitInput target v
            _ -> empty
