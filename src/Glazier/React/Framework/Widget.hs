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
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Kind
import Data.JSString as JS
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Commands.Rerender as C
import qualified JavaScript.Extras as JE

-- FIXME: hide constructor
-- newtype Inactive a = Inactive a

----------------------------------------------------------

class HasProperties c where
    properties :: Lens' c [JE.Property]

instance HasProperties [JE.Property] where
    properties = id

----------------------------------------------------------

-- | Plan has to be stored differently to other plans because mkPlan needs
-- additional parameters

newtype FrameNum = FrameNum Int deriving R.Dispose

data ComponentPlan = ComponentPlan
    { _component :: R.ReactComponent
    , _onRender ::  J.Callback (IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''ComponentPlan

instance R.Dispose ComponentPlan

data Plan = Plan
    { _frameNum :: FrameNum
    , _componentRef :: C.ComponentRef
    , _deferredDisposables :: R.Disposable ()
    , _listeners :: [R.Listener]
    , _key :: J.JSString
    , _componentPlan' :: Maybe ComponentPlan
    } deriving (G.Generic)

makeClassy ''Plan

instance R.Dispose Plan

----------------------------------------------------------

class HasSpecifications c specs | c -> specs where
    specifications :: Lens' c (Many specs)

----------------------------------------------------------

newtype Design (specs :: [Type]) = Design
    { runDesign ::
        ( [JE.Property]
        , Many specs
        , Plan
        )
    }
    deriving G.Generic

-- | UndecidableInstances, but safe because @Many specs@ is smaller than @Design specs@
instance (R.Dispose (Many specs)) => R.Dispose (Design specs)

instance HasPlan (Design specs) where
    plan = _Design . _3

instance HasProperties (Design specs) where
    properties = _Design . _1

instance HasSpecifications (Design specs) specs where
    specifications = _Design . _2

_Design :: Iso
    (Design specs)
    (Design specs')
    ([JE.Property], Many specs, Plan)
    ([JE.Property], Many specs', Plan)
_Design = iso runDesign Design

----------------------------------------------------------

-- FIXME: Move to STM extras
usingTVar :: TVar s -> StateT s STM a -> STM a
usingTVar v m = do
    s <- readTVar v
    (a, s') <- runStateT m s
    writeTVar v s'
    pure a

usingTVar' :: MFunctor t => TVar s -> t (StateT s STM) a -> (t STM) a
usingTVar' v = hoist (usingTVar v)

viewingTVar :: TVar s -> (s -> STM r) -> STM r
viewingTVar v m = readTVar v >>= m

viewingTVar' :: (MonadTrans t, Monad (t STM)) => TVar s -> (s -> (t STM) r) -> (t STM) r
viewingTVar' v m = lift (readTVar v) >>= m

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

doOnComponentDidUpdate :: StateT Plan STM (R.Disposable ())
doOnComponentDidUpdate = do
    -- Run delayed commands that need to wait until frame is re-rendered
    -- Eg focusing after other rendering changes
    ds <- use deferredDisposables
    deferredDisposables .= mempty
    pure ds

queueDisposable :: R.Dispose a => a -> StateT (Design specs) STM ()
queueDisposable a = (plan . deferredDisposables) %= (>> R.dispose a)

inactivePlan
    :: Plan
inactivePlan = Plan
    (FrameNum 0) -- frameNum
    (C.ComponentRef J.nullRef) -- componentRef
    mempty -- deferredDisposables
    mempty -- traverse go ts -- triggers
    mempty -- R.mkKey' -- key
    Nothing -- componentPlan

mkComponentPlan
    :: (Design specs -> R.ReactMlT STM ())
    -> TVar (Design specs)
    -> F R.Reactor (ComponentPlan)
mkComponentPlan w v = ComponentPlan
    <$> R.getComponent -- component
    <*> R.mkRenderer rnd -- onRender
    <*> R.mkCallback (atomically . usingTVar v . zoom plan . doOnComponentRef) -- onComponentRef
    <*> (R.mkCallback $ (>>= R.runDisposable)
            . atomically
            . usingTVar v
            . zoom plan
            . const doOnComponentDidUpdate) --onComopnentDidUpdate
  where
    rnd = viewingTVar' v w

initPlan
    :: (Design specs -> R.ReactMlT STM ())
    -> TVar (Design specs)
    -> Plan
    -> F R.Reactor (Maybe Plan)
initPlan _ _ (Plan _ _ _ _ _ (Just _)) = pure Nothing
initPlan w v (Plan frm cRef defDisp ls k Nothing) = fmap Just $ Plan frm cRef defDisp ls
    <$> ((k `JS.append`) <$> R.mkKey') -- key
    <*> (Just <$> mkComponentPlan w v)

mkListeners
    :: (Which cmds -> MaybeT IO ())
    -> [(J.JSString, J.JSVal -> MaybeT IO (DL.DList (Which cmds)))]
    -> F R.Reactor [R.Listener]
mkListeners exec = traverse toListener -- triggers
  where
    toListener (n, f) = (\a -> (n, a)) <$> R.mkCallback (toCallback f)
    toCallback f a = void . runMaybeT $ f a >>= traverse_ exec

-- attachListenersToPlan :: [R.Listener]

-- mkPlan
--     :: (Design specs -> R.ReactMlT STM ())
--     -> [(J.JSString, J.JSVal -> IO ())]
--     -> TMVar s
--     -> Lens' s (Design specs)
--     -> F R.Reactor Plan
-- mkPlan w ts v l = Plan
--     <$> R.mkKey' -- key
--     <*> pure (FrameNum 0) -- frameNum
--     <*> R.getComponent -- component
--     <*> pure (C.ComponentRef J.nullRef) -- componentRef
--     <*> pure mempty -- deferredDisposables
--     <*> R.mkRenderer rnd -- onRender
--     <*> R.mkCallback (atomically . usingTMVar v (l . plan) . doOnComponentRef) -- onComponentRef
--     <*> (do
--             dsp <- R.getDisposer
--             R.mkCallback $ atomically
--                 . (>>= void . PC.send dsp)
--                 . usingTMVar v (l . plan)
--                 . const doOnComponentDidUpdate) --onComopnentDidUpdate
--     <*> traverse go ts -- triggers
--   where
--     rnd = lift (view l <$> takeTMVar v) >>= w
--     go (n, f) = (\a -> (n, a)) <$> R.mkCallback f

inactiveDesign
    :: [JE.Property]
    -> Many specs
    -> Design specs
inactiveDesign ps specs = Design (ps, specs, inactivePlan)

initDesign
    :: (Design specs -> R.ReactMlT STM ())
    -> TVar (Design specs)
    -> MaybeT (F R.Reactor) (Design specs)
initDesign w v = do
    dsgn <- lift . R.doSTM $ readTVar v
    pln <- MaybeT $ initPlan w v (dsgn ^. plan)
    pure (dsgn & plan .~ pln)
    -- R.doSTM $ writeTVar v' (dsgn & plan .~ pln)
    -- pure v'

-- activateDesign
--     :: (Design specs -> R.ReactMlT STM ())
--     -> TVar (Design specs)
--     -> F R.Reactor (Design specs)
-- activateDesign exec ts w v = do
--     dsgn <- R.doSTM (readTVar v)
--     let v' = coerce v
--     pln <- activatePlan exec ts w v' (dsgn ^. plan)
--     pure (dsgn & plan .~ pln)
--     -- R.doSTM $ writeTVar v' (dsgn & plan .~ pln)
--     -- pure v'


-- mkDesign
--     :: [JE.Property] 
--     -> Many specs
--     -> (Which cmds -> MaybeT IO ())
--     -> [(J.JSString, TVar (Design specs) -> J.JSVal -> MaybeT IO (DL.DList (Which cmds)))]
--     -> (Design specs -> R.ReactMlT STM ())
--     -> F R.Reactor (TVar (Design specs))
-- mkDesign ps ss exec cbs w = do
--     d <- mkInactiveDesign ps ss
--     v <- R.doSTM $ newTVar d
--     d' <- activateDesign exec cbs w v
--     R.doSTM $ writeTVar v d'
--     pure v

componentWindow :: Design specs -> R.ReactMlT STM ()
componentWindow s =
    let cPlan = s ^. plan . componentPlan'
    in case cPlan of
        Nothing -> pure ()
        Just cPlan' -> R.lf
           (cPlan' ^. component . to JE.toJS')
            [ ("ref", cPlan' ^. onComponentRef)
            , ("componentDidUpdate", cPlan' ^. onComponentDidUpdate)
            ]
            [ ("key", s ^. plan . key . to JE.toJS')
            -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
            , ("render", cPlan' ^. onRender . to JE.toJS')
            ]
