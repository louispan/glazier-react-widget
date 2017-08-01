{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input
    ( InputAction(..)
    , inputPrototype
    ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands as C
import qualified Glazier.React.Triggers as T
import qualified JavaScript.Extras as JE

data InputAction
    = SubmitAction R.EventTarget J.JSString
    | CancelAction R.EventTarget

inputPrototype
    :: ( UniqueMember T.KeyDownKeyTrigger trigs
       , UniqueMember InputAction acts
       , UniqueMember C.SetPropertyCommand cmds)
    => F.Prototype IO u
                  '[] ols
                  '[] dtls
                  '[] plns
                  '[T.KeyDownKeyTrigger] trigs
                  '[InputAction] '[InputAction] acts
                  '[C.SetPropertyCommand] '[C.SetPropertyCommand] cmds
                  '[] envs
inputPrototype = F.Prototype ( F.idle
                             , F.display d
                             , F.trigger "onKeyDown" T.keyDownKeyTrigger go
                             , F.gadgetry gadget
                             , C.execProperty
                             )
  where
    d ls ps = lift $ R.lf "input" ls ps
    go (T.KeyDownKeyTrigger target k) = case k of
        "Escape" -> pure $ CancelAction target
        "Enter" -> do
            v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
            pure $ SubmitAction target v
        _ -> empty

gadget :: G.Gadget InputAction (F.Entity dtls plns) (D.DList C.SetPropertyCommand)
gadget = do
    a <- ask
    case a of
        CancelAction j -> pure $ D.singleton $ C.SetPropertyCommand (JE.toJS j) ("value", JE.toJS' J.empty)
        _ -> empty
