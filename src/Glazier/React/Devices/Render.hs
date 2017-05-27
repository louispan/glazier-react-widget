{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Devices.Render
    ( Command(..)
    , Action(..)
    , Plan(..)
    , HasPlan(..)
    , Device
    , device
    ) where

import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Control.Disposable as CD
import qualified Data.DList as D
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified GHC.Generics as G
import qualified Glazier as G
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

data Command mdl = RenderCommand (R.Shared mdl) [JE.Property] J.JSVal

data Action
    = ComponentRefAction J.JSVal
    | RenderAction

data Plan = Plan
    { _frameNum :: Int
    , _componentRef :: J.JSVal
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan :: F (R.Maker Action) Plan
mkPlan = Plan
    <$> pure 0
    <*> pure J.nullRef
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)

instance CD.Disposing Plan

instance HasPlan pln => HasPlan (R.Model dtl pln) where
    plan = R.plan . plan

-- | Undecidableinstances! This is safe because pln is smaller than mdl
instance (R.HasModel mdl dtl pln, HasPlan pln) => HasPlan (R.Shared mdl) where
    plan = R.plan . plan

windowAttrs :: (R.HasModel mdl dtl pln, HasPlan pln) => mdl -> R.WindowAttrs
windowAttrs mdl = R.WindowAttrs (mempty, [("ref", mdl ^. R.model . onComponentRef)])

gadget :: (R.HasModel mdl dtl pln, HasPlan pln) => G.Gadget Action (R.Shared mdl) (D.DList (Command mdl))
gadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        RenderAction -> do
            -- Just change the state to a different number so the React pureComponent will call render()
            frameNum %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
            i <- JE.toJS <$> use frameNum
            r <- use componentRef
            s <- get
            pure . D.singleton $ RenderCommand s [("frameNum", JE.JSVar i)] r

type Device mdl = R.Device Action Plan (Command mdl) mdl

device :: (R.HasModel mdl dtl pln, HasPlan pln) => R.Device Action Plan (Command mdl) mdl
device = R.Device (const mkPlan) gadget windowAttrs (const mempty)
