{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Triggers.EventTarget
    ( Action(..)
    , Plan(..)
    , HasPlan(..)
    , Trigger
    , trigger
    ) where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

newtype Action = EventTargetAction R.DOMEventTarget

newtype Plan = Plan
    { _onTrigger :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan :: F (R.Maker Action) Plan
mkPlan = Plan <$> (R.mkHandler onTrigger')

instance CD.Disposing Plan

renderAttributes :: J.JSString -> Lens' mdl Plan -> mdl -> R.RenderAttributes
renderAttributes evt pln mdl = R.RenderAttributes (mempty, [(evt, mdl ^. pln . onTrigger)])

onTrigger' :: J.JSVal -> MaybeT IO [Action]
onTrigger' = R.eventHandlerM strictly lazily
  where
    strictly evt = MaybeT . pure $ JE.fromJS evt <&> (R.target . R.parseEvent)
    lazily :: R.DOMEventTarget -> MaybeT IO [Action]
    lazily j = pure [EventTargetAction j]


type Trigger mdl = R.Trigger Action Plan mdl

trigger
    :: J.JSString
    -> Lens' mdl Plan
    -> Trigger mdl
trigger evt pln = R.Trigger
    pln
    mkPlan
    (renderAttributes evt pln)
