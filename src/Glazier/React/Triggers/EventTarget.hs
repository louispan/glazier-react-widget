{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Triggers.EventTarget where

import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified Data.JSString as JS
import Data.Proxy
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

newtype Action t = EventTargetAction R.EventTarget

newtype Plan t = Plan
    { _onTrigger :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan :: F (R.Maker (Action t)) (Plan t)
mkPlan = Plan <$> (R.mkHandler onTrigger')
  where
    onTrigger' :: J.JSVal -> MaybeT IO [Action t]
    onTrigger' = R.eventHandlerM strictly lazily
      where
        strictly evt = MaybeT . pure $ JE.fromJS evt <&> (R.target . R.parseEvent)
        lazily j = pure [EventTargetAction j]

instance R.Dispose (Plan t)

onEventName :: Show t => t -> J.JSString
onEventName t = "on" `JS.append` (JS.pack $ show t)

-- | @evt@ examples: onKeyDown, onChanged etc
toWindowListeners
    :: forall t dtls plns.
       (Show t, UniqueMember (Plan t) plns)
    => t -> R.ToWindowListeners dtls plns
toWindowListeners t p = D.singleton $ R.WindowListener (onEventName t, p ^. R.plans . item @(Plan t) . onTrigger)

widget
    :: (Show t, UniqueMember (Plan t) plns, UniqueMember (Action t) acts)
    => t -> R.Widget '[] '[] '[Plan t] '[Action t] '[] ols dtls plns v acts cmds
widget t =
    ( Proxy
    , Proxy
    , R.Display (mempty, toWindowListeners t, Nothing)
    , R.Gizmo
          ( const $ pure nil
          , const nil
          , single <$> R.hoistWithAction pick mkPlan
          , mempty))

-- -- | Given the Tag of the event (KeyDown, Changed), fire an Action that contains the EventTarget.
-- trigger :: forall t mdls. (Show t, UniqueMember (Plan t) mdls) => t -> Trigger t (Many mdls)
-- trigger t = trigger' (onEventName t) (item @(Plan t))
--   where
--     trigger'
--         :: J.JSString
--         -> Lens' mdl (Plan t)
--         -> Trigger t mdl
--     trigger' evt pln = R.Trigger
--         pln
--         mkPlan
--         (renderAttributes evt pln)

-- Tags for event targets
data KeyDown = KeyDown deriving (Show, Eq)
data Changed = Changed deriving (Show, Eq)
