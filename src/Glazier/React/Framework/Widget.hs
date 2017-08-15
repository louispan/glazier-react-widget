{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Framework.Widget where
--   ( WidgetCommand(..)
--   , WidgetAction(..)
--   , HasProperties(..)
--   , HasWidgetPlan(..)
--   , HasDetails(..)
--   , HasPlans(..)
--   , Design(..)
--   , _Design
--   , Entity
--   , withTMVar
--   , inTMVar
--   , widgetGadget
--   , widgetWindow
--   , putEntity
--   ) where
-- -- We want to hide mkWidgetPlan

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import Data.Kind
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC

----------------------------------------------------------

class HasProperties c where
    properties :: Lens' c [JE.Property]

instance HasProperties [JE.Property] where
    properties = id

----------------------------------------------------------

-- | WidgetPlan has to be stored differently to other plans because mkWidgetPlan needs
-- additional parameters
newtype ComponentRef = ComponentRef J.JSVal deriving R.Dispose
newtype FrameNum = FrameNum Int deriving R.Dispose

data WidgetPlan = WidgetPlan
    { _key :: J.JSString
    , _frameNum :: FrameNum
    , _component :: R.ReactComponent
    , _componentRef :: ComponentRef
    , _deferredDisposables :: R.Disposable ()
    , _onRender ::  J.Callback (IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    , _listeners :: [R.Listener]
    } deriving (G.Generic)

makeClassy ''WidgetPlan

instance R.Dispose WidgetPlan

----------------------------------------------------------

class HasDetails c dtls | c -> dtls where
    details :: Lens' c (Many dtls)

class HasPlans c plns | c -> plns where
    plans :: Lens' c (Many plns)

----------------------------------------------------------

newtype Design (dtls :: [Type]) (plns :: [Type]) = Design
    { getDesign ::
        ( [JE.Property]
        , Many dtls
        , WidgetPlan
        , Many plns
        )
    }
    deriving G.Generic

instance (R.Dispose (Many dtls), R.Dispose (Many plns)) => R.Dispose (Design dtls plns)

instance HasProperties (Design dtls plns) where
    properties = _Design . _1

instance HasDetails (Design dtls plns) dtls where
    details = _Design . _2

instance HasWidgetPlan (Design dtls plns) where
    widgetPlan = _Design . _3

instance HasPlans (Design dtls plns) plns where
    plans = _Design . _4

_Design :: Iso
    (Design dtls plns)
    (Design dtls' plns')
    ([JE.Property], Many dtls, WidgetPlan, Many plns)
    ([JE.Property], Many dtls', WidgetPlan, Many plns')
_Design = iso getDesign Design

----------------------------------------------------------

type Entity dtls plns = TMVar (Design dtls plns)

withTMVar :: TMVar s -> StateT s STM a -> STM a
withTMVar v m = do
    s <- takeTMVar v
    (a, s') <- runStateT m s
    putTMVar v s'
    pure a

-- | Send widget output into a queue to be process by a separate worker.
post :: PC.Output o -> o -> STM ()
post o = void . PC.send o

----------------------------------------------------------

data Rerender = Rerender ComponentRef [JE.Property]

rerender :: StateT (Design dtls plns) STM Rerender
rerender = do
    -- Just change the state to a different number so the React PureComponent will call render()
    (widgetPlan . frameNum) %= (\(FrameNum i) -> FrameNum $ (i `mod` JE.maxSafeInteger) + 1)
    FrameNum i <- use (widgetPlan . frameNum)
    r <- use (widgetPlan . componentRef)
    pure $ Rerender r [("frameNum", JE.JSVar $ JE.toJS i)]

----------------------------------------------------------

doOnComponentRef :: J.JSVal -> StateT (Design dtls plns) STM ()
doOnComponentRef j = (widgetPlan . componentRef) .= ComponentRef j

doOnComponentDidUpdate :: StateT (Design dtls plns) STM (R.Disposable ())
doOnComponentDidUpdate = do
    -- Run delayed commands that need to wait until frame is re-rendered
    -- Eg focusing after other rendering changes
    ds <- use (widgetPlan . deferredDisposables)
    (widgetPlan . deferredDisposables) .= mempty
    pure ds

type Trigger dtls plns  = (J.JSString, Entity dtls plns -> J.JSVal -> IO ())

mkWidgetPlan
    :: PC.Output (R.Disposable ())
    -> (Design dtls plns -> R.ReactMlT STM ())
    -> [Trigger dtls plns]
    -> Entity dtls plns
    -> F R.Reactor WidgetPlan
mkWidgetPlan disp w ts v = WidgetPlan
    <$> R.mkKey' -- key
    <*> pure (FrameNum 0) -- frameNum
    <*> R.getComponent -- component
    <*> pure (ComponentRef J.nullRef) -- componentRef
    <*> pure mempty -- deferredDisposables
    <*> (R.mkRenderer rnd) -- onRender
    <*> R.mkHandler (atomically . withTMVar v . doOnComponentRef) -- onComponentRef
    <*> R.mkHandler (atomically . (>>= post disp) . withTMVar v . const doOnComponentDidUpdate) --onComopnentDidUpdate
    <*> (traverse go ts) -- triggers
  where
    rnd = lift (takeTMVar v) >>= w
    go (n, f) = (\a -> (n, a)) <$> R.mkHandler (f v)

mkDesign
    :: Entity dtls plns -- This must be empty!
    -> PC.Output (R.Disposable ())
    -> (Design dtls plns -> R.ReactMlT STM ())
    -> [Trigger dtls plns]
    -> [JE.Property]
    -> Many dtls
    -> F R.Reactor (Many plns)
    -> F R.Reactor (Design dtls plns)
mkDesign v disp w hdls ps dtls mkPlns = (\plns compPln -> Design (ps, dtls, plns, compPln)) <$> mkWidgetPlan disp w hdls v <*> mkPlns

componentWindow :: Design dtls plns -> R.ReactMlT STM ()
componentWindow s = do
    R.lf
        (s ^. widgetPlan  . component . to JE.toJS')
        [ ("ref", s ^. widgetPlan . onComponentRef)
        , ("componentDidUpdate", s ^. widgetPlan . onComponentDidUpdate)
        ]
        [ ("key", s ^. widgetPlan . key . to JE.toJS')
        -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
        , ("render", s ^. widgetPlan . onRender . to JE.toJS')
        ]

-- data Widget s i o = Widget
--     { input :: TVar (G.GizmoT i STM ())
--     , output :: TVar (MaybeT STM o)
--     , model :: TVar s
--     , gadget :: G.GizmoT i (StateT s STM) o
--     , window :: G.GizmoT s (R.ReactMlT STM) ()
--     -- , builder ::
--     -- Executors
--     }
