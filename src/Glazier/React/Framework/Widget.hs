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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Widget where
--   ( WidgetCommand(..)
--   , WidgetAction(..)
--   , HasProperties(..)
--   , HasPlan(..)
--   , HasSpecifications(..)
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
-- -- We want to hide mkPlan

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
import qualified Glazier.React.Commands.Rerender as C
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC

----------------------------------------------------------

class HasProperties c where
    properties :: Lens' c [JE.Property]

instance HasProperties [JE.Property] where
    properties = id

----------------------------------------------------------

-- | Plan has to be stored differently to other plans because mkPlan needs
-- additional parameters

newtype FrameNum = FrameNum Int deriving R.Dispose

data Plan = Plan
    { _key :: J.JSString
    , _frameNum :: FrameNum
    , _component :: R.ReactComponent
    , _componentRef :: C.ComponentRef
    , _deferredDisposables :: R.Disposable ()
    , _onRender ::  J.Callback (IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    , _listeners :: [R.Listener]
    } deriving (G.Generic)

makeClassy ''Plan

instance R.Dispose Plan

----------------------------------------------------------

class HasSpecifications c specs | c -> specs where
    specifications :: Lens' c (Many specs)

----------------------------------------------------------

newtype Design (specs :: [Type]) = Design
    { unDesign ::
        ( Plan
        , [JE.Property]
        , Many specs)
    }
    deriving G.Generic

-- | UndecidableInstances, but safe because @Many specs@ is smaller than @Design specs@
instance (R.Dispose (Many specs)) => R.Dispose (Design specs)

instance HasPlan (Design specs) where
    plan = _Design . _1

instance HasProperties (Design specs) where
    properties = _Design . _2

instance HasSpecifications (Design specs) specs where
    specifications = _Design . _3

_Design :: Iso
    (Design specs)
    (Design specs')
    (Plan, [JE.Property], Many specs)
    (Plan, [JE.Property], Many specs')
_Design = iso unDesign Design

----------------------------------------------------------

usingTMVar :: TMVar s -> Lens' s p -> StateT p STM a -> STM a
usingTMVar v l m = do
    s <- takeTMVar v
    let t = s ^. l
    (a, t') <- runStateT m t
    putTMVar v (s & l .~ t')
    pure a

viewingTMVar :: TMVar s -> (s -> STM r) -> STM r
viewingTMVar v frmEnt = readTMVar v >>= frmEnt

viewingTMVar' :: (MonadTrans t, Monad (t STM)) =>TMVar s -> (s -> (t STM) r) -> (t STM) r
viewingTMVar' v frmEnt = lift (readTMVar v) >>= frmEnt

-- | Send widget output into a queue to be process by a separate worker.
-- post :: PC.Output o -> o -> STM ()
-- post o = void . PC.send o

----------------------------------------------------------

rerender :: StateT (Design specs) STM C.Rerender
rerender = do
    -- Just change the state to a different number so the React PureComponent will call render()
    (plan . frameNum) %= (\(FrameNum i) -> FrameNum $ (i `mod` JE.maxSafeInteger) + 1)
    FrameNum i <- use (plan . frameNum)
    r <- use (plan . componentRef)
    pure $ C.Rerender r [("frameNum", JE.JSVar $ JE.toJS i)]

----------------------------------------------------------

doOnComponentRef :: J.JSVal -> StateT Plan STM ()
doOnComponentRef j = componentRef .= C.ComponentRef j

-- doOnComponentRef' :: TMVar s -> Lens' s Plan -> J.JSVal -> IO ()
-- doOnComponentRef' v l = atomically . usingTMVar v l . doOnComponentRef

doOnComponentDidUpdate :: StateT Plan STM (R.Disposable ())
doOnComponentDidUpdate = do
    -- Run delayed commands that need to wait until frame is re-rendered
    -- Eg focusing after other rendering changes
    ds <- use deferredDisposables
    deferredDisposables .= mempty
    pure ds

-- doOnComponentDidUpdate' :: PC.Output (R.Disposable ()) -> TMVar s -> Lens' s Plan -> J.JSVal -> IO ()
-- doOnComponentDidUpdate' dc v l = atomically . (>>= void . PC.send dc) . usingTMVar v l . const doOnComponentDidUpdate

-- type Trigger specs = (J.JSString, TMVar (Design specs) -> J.JSVal -> IO ())

-- type Delegate a specs = (TMVar (Design specs) -> Which a -> STM ()) -> (J.JSString, TMVar (Design specs) -> J.JSVal -> IO ())

mkPlan
    :: (Design specs -> R.ReactMlT STM ())
    -> (TMVar s -> Lens' s (Design specs) -> [(J.JSString, J.JSVal -> IO ())])
    -> TMVar s
    -> Lens' s (Design specs)
    -> F R.Reactor Plan
mkPlan w ts v l = Plan
    <$> R.mkKey' -- key
    <*> pure (FrameNum 0) -- frameNum
    <*> R.getComponent -- component
    <*> pure (C.ComponentRef J.nullRef) -- componentRef
    <*> pure mempty -- deferredDisposables
    <*> R.mkRenderer rnd -- onRender
    <*> R.mkCallback (atomically . usingTMVar v (l . plan) . doOnComponentRef) -- onComponentRef
    <*> (do
            dsp <- R.getDisposer
            R.mkCallback $ atomically
                . (>>= void . PC.send dsp)
                . usingTMVar v (l . plan)
                . const doOnComponentDidUpdate) --onComopnentDidUpdate
    <*> traverse go ts' -- triggers
  where
    ts' = ts v l
    rnd = lift (view l <$> takeTMVar v) >>= w
    go (n, f) = (\a -> (n, a)) <$> R.mkCallback f

mkDesign
    :: [JE.Property]
    -> Many specs
    -> (Design specs -> R.ReactMlT STM ())
    -> (TMVar s -> Lens' s (Design specs) -> [(J.JSString, J.JSVal -> IO ())])
    -> TMVar s -- (Design specs) -- This must be empty!
    -> Lens' s (Design specs)
    -> F R.Reactor (Design specs)
mkDesign ps specs w ts v l = (\pln -> Design (pln, ps, specs)) <$> mkPlan w ts v l

componentWindow :: Design specs -> R.ReactMlT STM ()
componentWindow s =
    R.lf
        (s ^. plan  . component . to JE.toJS')
        [ ("ref", s ^. plan . onComponentRef)
        , ("componentDidUpdate", s ^. plan . onComponentDidUpdate)
        ]
        [ ("key", s ^. plan . key . to JE.toJS')
        -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
        , ("render", s ^. plan . onRender . to JE.toJS')
        ]
