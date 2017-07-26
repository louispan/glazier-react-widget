{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Framework.Widget where

import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Shared as F
import qualified Glazier.React.Framework.Trigger as F
import qualified JavaScript.Extras as JE

data WidgetCommand
    = forall i v. RenderCommand (F.Shared i v) [JE.Property] J.JSVal
    | DisposeCommand (R.Disposable ())

data WidgetAction
    = ComponentRefAction J.JSVal
    | RenderAction
    | DisposeAction
----------------------------------------------------------

newtype WidgetDetail = WidgetDetail
    { _properties :: [JE.Property]
    } deriving (G.Generic)

class HasWidgetDetail c where
    widgetDetail :: Lens' c WidgetDetail
    properties :: Lens' c [JE.Property]
    properties = widgetDetail . go
      where go k (WidgetDetail a) = k a <&> \a' -> WidgetDetail a'

instance HasWidgetDetail WidgetDetail where
    widgetDetail = id

instance R.Dispose WidgetDetail where
    dispose _ = pure ()

----------------------------------------------------------

-- | WidgetPlan has to be stored differently to other plans because mkWidgetPlan needs
-- additional parameters
data WidgetPlan = WidgetPlan
    { _key :: J.JSString
    , _frameNum :: Int
    , _component :: R.ReactComponent
    , _componentRef :: J.JSVal
    , _deferredDisposables :: R.Disposable ()
    , _onRender ::  J.Callback (IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    , _listeners :: [R.Listener]
    } deriving (G.Generic)

makeClassy ''WidgetPlan

mkWidgetPlan
    :: UniqueMember WidgetAction acts
    => G.WindowT mdl R.ReactMl ()
    -> TMVar mdl
    -> [(J.JSString, F.TriggerAction -> [Which acts])]
    -> F (R.Maker (Which acts)) WidgetPlan
mkWidgetPlan render frm ts = R.hoistWithAction pick (WidgetPlan
    <$> R.mkKey -- key
    <*> pure 0 -- frameNum
    <*> R.getComponent -- component
    <*> pure J.nullRef -- componentRef
    <*> pure mempty -- deferredDisposables
    <*> (R.mkRenderer render frm) -- onRender
    <*> (R.mkHandler $ pure . pure . ComponentRefAction) -- onComponentRef
    <*> (R.mkHandler $ pure . pure . const DisposeAction) -- onComponentDidUpdate
    )
    <*> (traverse go . M.toList . M.fromListWith (<>) $ ts) -- triggers
  where
    go (n, f) = (\a -> (n, a)) <$> R.mkHandler (fmap f <$> F.onTrigger n)

instance R.Dispose WidgetPlan

----------------------------------------------------------

class HasDetails c dtls | c -> dtls where
    details :: Lens' c (Many dtls)

class HasPlans c plns | c -> plns where
    plans :: Lens' c (Many plns)

----------------------------------------------------------

newtype Design (dtls :: [Type]) (plns :: [Type]) = Design
    { getDesign ::
        ( Many dtls
        , WidgetDetail
        , Many plns
        , WidgetPlan
        )
    }
    deriving G.Generic

instance (R.Dispose (Many dtls), R.Dispose (Many plns)) => R.Dispose (Design dtls plns)

instance HasDetails (Design dtls plns) dtls where
    details = _Design . _1

instance HasWidgetDetail (Design dtls plns) where
    widgetDetail = _Design . _2

instance HasPlans (Design dtls plns) plns where
    plans = _Design . _3

instance HasWidgetPlan (Design dtls plns) where
    widgetPlan = _Design . _4

_Design :: Iso
    (Design dtls plns)
    (Design dtls' plns')
    (Many dtls, WidgetDetail, Many plns, WidgetPlan)
    (Many dtls', WidgetDetail, Many plns', WidgetPlan)
_Design = iso getDesign Design

----------------------------------------------------------

type Entity dtls plns v = F.Shared (Design dtls plns) v
type Entity' dtls plns = F.Shared (Design dtls plns) (Design dtls plns)

instance HasDetails (Entity dtls plns v) dtls where
    details = F.ival . details

instance HasWidgetDetail (Entity dtls plns v) where
    widgetDetail = F.ival . widgetDetail

instance HasPlans (Entity dtls plns v) plns where
    plans = F.ival . plans

instance HasWidgetPlan (Entity dtls plns v) where
    widgetPlan = F.ival . widgetPlan

----------------------------------------------------------

componentGadget :: G.Gadget WidgetAction (Entity dtls plns v) (D.DList WidgetCommand)
componentGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            (widgetPlan . componentRef) .= node
            pure mempty

        RenderAction -> do
            -- Just change the state to a different number so the React PureComponent will call render()
            (widgetPlan . frameNum) %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
            i <- JE.toJS <$> use (widgetPlan . frameNum)
            r <- use (widgetPlan . componentRef)
            s <- get
            pure . D.singleton $ RenderCommand s [("frameNum", JE.JSVar i)] r

        DisposeAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            ds <- use (widgetPlan . deferredDisposables)
            (widgetPlan . deferredDisposables) .= mempty
            pure . D.singleton . DisposeCommand $ ds

componentWindow :: G.WindowT (Design dtls plns) R.ReactMl ()
componentWindow = do
    s <- ask
    lift $
        R.lf
            (s ^. widgetPlan  . component . to JE.toJS')
            [ ("ref", s ^. widgetPlan . onComponentRef)
            , ("componentDidUpdate", s ^. widgetPlan . onComponentDidUpdate)
            ]
            [ ("key", s ^. widgetPlan . key . to JE.toJS')
            -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
            , ("render", s ^. widgetPlan . onRender . to JE.toJS')
            ]

----------------------------------------------------------

-- | Make a Entity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkEntity'
    :: UniqueMember WidgetAction acts
    => Many dtls
    -> [JE.Property]
    -> [(J.JSString, F.TriggerAction -> [Which acts])]
    -> F (R.Maker (Which acts)) (Many plns)
    -> G.WindowT (Design dtls plns) R.ReactMl ()
    -> F (R.Maker (Which acts)) (Entity' dtls plns)
mkEntity' dtls ps ts mkPlns render = do
    frm <- R.mkEmptyFrame
    mdl <- (\plns compPln -> Design (dtls, WidgetDetail ps, plns, compPln)) <$> mkPlns <*> mkWidgetPlan render frm ts
    R.putFrame frm mdl
    pure $ F.Shared (mdl, Lens id, frm)
