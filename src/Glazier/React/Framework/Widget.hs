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
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Widget where
--   ( WidgetCommand(..)
--   , WidgetAction(..)
--   , HasProperties(..)
--   , HasPlan(..)
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
newtype ComponentRef = ComponentRef J.JSVal deriving R.Dispose
newtype FrameNum = FrameNum Int deriving R.Dispose

data Plan = Plan
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

makeClassy ''Plan

instance R.Dispose Plan

----------------------------------------------------------

class HasDetails c dtls | c -> dtls where
    details :: Lens' c (Many dtls)

----------------------------------------------------------

newtype Design (dtls :: [Type]) = Design
    { unDesign ::
        ( Plan
        , [JE.Property]
        , Many dtls)
    }
    deriving G.Generic

-- | UndecidableInstances, but safe because @Many dtls@ is smaller than @Design dtls@
instance (R.Dispose (Many dtls)) => R.Dispose (Design dtls)

instance HasPlan (Design dtls) where
    plan = _Design . _1

instance HasProperties (Design dtls) where
    properties = _Design . _2

instance HasDetails (Design dtls) dtls where
    details = _Design . _3

_Design :: Iso
    (Design dtls)
    (Design dtls')
    (Plan, [JE.Property], Many dtls)
    (Plan, [JE.Property], Many dtls')
_Design = iso unDesign Design

----------------------------------------------------------

-- type Entity dtls = TMVar (Design dtls)

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

rerender :: StateT (Design dtls) STM Rerender
rerender = do
    -- Just change the state to a different number so the React PureComponent will call render()
    (plan . frameNum) %= (\(FrameNum i) -> FrameNum $ (i `mod` JE.maxSafeInteger) + 1)
    FrameNum i <- use (plan . frameNum)
    r <- use (plan . componentRef)
    pure $ Rerender r [("frameNum", JE.JSVar $ JE.toJS i)]

----------------------------------------------------------

doOnComponentRef :: J.JSVal -> StateT (Design dtls) STM ()
doOnComponentRef j = (plan . componentRef) .= ComponentRef j

doOnComponentDidUpdate :: StateT (Design dtls) STM (R.Disposable ())
doOnComponentDidUpdate = do
    -- Run delayed commands that need to wait until frame is re-rendered
    -- Eg focusing after other rendering changes
    ds <- use (plan . deferredDisposables)
    (plan . deferredDisposables) .= mempty
    pure ds

type Trigger dtls  = (J.JSString, TMVar (Design dtls) -> J.JSVal -> IO ())

mkPlan
    :: PC.Output (R.Disposable ())
    -> (Design dtls -> R.ReactMlT STM ())
    -> [Trigger dtls]
    -> TMVar (Design dtls)
    -> F R.Reactor Plan
mkPlan disp w ts v = Plan
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
    :: TMVar (Design dtls) -- This must be empty!
    -> PC.Output (R.Disposable ())
    -> (Design dtls -> R.ReactMlT STM ())
    -> [Trigger dtls]
    -> [JE.Property]
    -> Many dtls
    -> F R.Reactor (Design dtls)
mkDesign v disp w hdls ps dtls = (\pln -> Design (pln, ps, dtls)) <$> mkPlan disp w hdls v

componentWindow :: Design dtls -> R.ReactMlT STM ()
componentWindow s = do
    R.lf
        (s ^. plan  . component . to JE.toJS')
        [ ("ref", s ^. plan . onComponentRef)
        , ("componentDidUpdate", s ^. plan . onComponentDidUpdate)
        ]
        [ ("key", s ^. plan . key . to JE.toJS')
        -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
        , ("render", s ^. plan . onRender . to JE.toJS')
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
