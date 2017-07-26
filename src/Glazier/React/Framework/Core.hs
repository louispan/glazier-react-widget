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

module Glazier.React.Framework.Core where

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

data ComponentCommand
    = forall i v. RenderCommand (F.Shared i v) [JE.Property] J.JSVal
    | DisposeCommand (R.Disposable ())

data ComponentAction
    = ComponentRefAction J.JSVal
    | RenderAction
    | DisposeAction
----------------------------------------------------------

newtype ComponentDetail = ComponentDetail
    { _properties :: [JE.Property]
    } deriving (G.Generic)

class HasComponentDetail c where
    componentDetail :: Lens' c ComponentDetail
    properties :: Lens' c [JE.Property]
    properties = componentDetail . go
      where go k (ComponentDetail a) = k a <&> \a' -> ComponentDetail a'

instance HasComponentDetail ComponentDetail where
    componentDetail = id

instance R.Dispose ComponentDetail where
    dispose _ = pure ()

----------------------------------------------------------

-- | ComponentPlan has to be stored differently to other plans because mkComponentPlan needs
-- additional parameters
data ComponentPlan = ComponentPlan
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

makeClassy ''ComponentPlan

mkComponentPlan
    :: UniqueMember ComponentAction acts
    => G.WindowT mdl R.ReactMl ()
    -> TMVar mdl
    -> [(J.JSString, F.TriggerAction -> [Which acts])]
    -> F (R.Maker (Which acts)) ComponentPlan
mkComponentPlan render frm ts = R.hoistWithAction pick (ComponentPlan
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

instance R.Dispose ComponentPlan

----------------------------------------------------------

class HasDetails c dtls | c -> dtls where
    details :: Lens' c (Many dtls)

class HasPlans c plns | c -> plns where
    plans :: Lens' c (Many plns)

----------------------------------------------------------

newtype Prototype (dtls :: [Type]) (plns :: [Type]) = Prototype
    { runPrototype ::
        ( Many dtls
        , ComponentDetail
        , Many plns
        , ComponentPlan
        )
    }
    deriving G.Generic

instance (R.Dispose (Many dtls), R.Dispose (Many plns)) => R.Dispose (Prototype dtls plns)

instance HasDetails (Prototype dtls plns) dtls where
    details = _Prototype . _1

instance HasPlans (Prototype dtls plns) plns where
    plans = _Prototype . _3

instance HasComponentDetail (Prototype dtls plns) where
    componentDetail = _Prototype . _2

instance HasComponentPlan (Prototype dtls plns) where
    componentPlan = _Prototype . _4

_Prototype :: Iso
    (Prototype dtls plns)
    (Prototype dtls' plns')
    (Many dtls, ComponentDetail, Many plns, ComponentPlan)
    (Many dtls', ComponentDetail, Many plns', ComponentPlan)
_Prototype = iso runPrototype Prototype

----------------------------------------------------------

type Entity dtls plns v = F.Shared (Prototype dtls plns) v
type Entity' dtls plns = F.Shared (Prototype dtls plns) (Prototype dtls plns)

instance HasDetails (Entity dtls plns v) dtls where
    details = F.ival . details

instance HasPlans (Entity dtls plns v) plns where
    plans = F.ival . plans

instance HasComponentPlan (Entity dtls plns v) where
    componentPlan = F.ival . componentPlan

----------------------------------------------------------

componentGadget :: G.Gadget ComponentAction (Entity dtls plns v) (D.DList ComponentCommand)
componentGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            (componentPlan . componentRef) .= node
            pure mempty

        RenderAction -> do
            -- Just change the state to a different number so the React PureComponent will call render()
            (componentPlan . frameNum) %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
            i <- JE.toJS <$> use (componentPlan . frameNum)
            r <- use (componentPlan . componentRef)
            s <- get
            pure . D.singleton $ RenderCommand s [("frameNum", JE.JSVar i)] r

        DisposeAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            ds <- use (componentPlan . deferredDisposables)
            (componentPlan . deferredDisposables) .= mempty
            pure . D.singleton . DisposeCommand $ ds

componentWindow :: G.WindowT (Prototype dtls plns) R.ReactMl ()
componentWindow = do
    s <- ask
    lift $
        R.lf
            (s ^. componentPlan  . component . to JE.toJS')
            [ ("ref", s ^. componentPlan . onComponentRef)
            , ("componentDidUpdate", s ^. componentPlan . onComponentDidUpdate)
            ]
            [ ("key", s ^. componentPlan . key . to JE.toJS')
            -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
            , ("render", s ^. componentPlan . onRender . to JE.toJS')
            ]

----------------------------------------------------------

-- | Make a Entity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkEntity'
    :: UniqueMember ComponentAction acts
    => Many dtls
    -> [JE.Property]
    -> [(J.JSString, F.TriggerAction -> [Which acts])]
    -> F (R.Maker (Which acts)) (Many plns)
    -> G.WindowT (Prototype dtls plns) R.ReactMl ()
    -> F (R.Maker (Which acts)) (Entity' dtls plns)
mkEntity' dtls ps ts mkPlns render = do
    frm <- R.mkEmptyFrame
    mdl <- (\plns compPln -> Prototype (dtls, ComponentDetail ps, plns, compPln)) <$> mkPlns <*> mkComponentPlan render frm ts
    R.putFrame frm mdl
    pure $ F.Shared (mdl, Lens id, frm)
