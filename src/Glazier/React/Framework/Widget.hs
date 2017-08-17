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

rerender :: StateT (Design specs) STM C.Rerender
rerender = do
    -- Just change the state to a different number so the React PureComponent will call render()
    (plan . frameNum) %= (\(FrameNum i) -> FrameNum $ (i `mod` JE.maxSafeInteger) + 1)
    FrameNum i <- use (plan . frameNum)
    r <- use (plan . componentRef)
    pure $ C.Rerender r [("frameNum", JE.JSVar $ JE.toJS i)]

----------------------------------------------------------

doOnComponentRef :: J.JSVal -> StateT (Design specs) STM ()
doOnComponentRef j = (plan . componentRef) .= C.ComponentRef j

doOnComponentDidUpdate :: StateT (Design specs) STM (R.Disposable ())
doOnComponentDidUpdate = do
    -- Run delayed commands that need to wait until frame is re-rendered
    -- Eg focusing after other rendering changes
    ds <- use (plan . deferredDisposables)
    (plan . deferredDisposables) .= mempty
    pure ds

type Trigger specs = (J.JSString, J.JSVal -> TMVar (Design specs) -> IO ())
type Delegate specs = TMVar (Design specs) -> STM ()

mkPlan
    :: PC.Output (R.Disposable ())
    -> (Design specs -> R.ReactMlT STM ())
    -> [Trigger specs]
    -> TMVar (Design specs)
    -> F R.Reactor Plan
mkPlan dc w ts v = Plan
    <$> R.mkKey' -- key
    <*> pure (FrameNum 0) -- frameNum
    <*> R.getComponent -- component
    <*> pure (C.ComponentRef J.nullRef) -- componentRef
    <*> pure mempty -- deferredDisposables
    <*> (R.mkRenderer rnd) -- onRender
    <*> R.mkHandler (atomically . withTMVar v . doOnComponentRef) -- onComponentRef
    <*> R.mkHandler (atomically . (>>= post dc) . withTMVar v . const doOnComponentDidUpdate) --onComopnentDidUpdate
    <*> (traverse go ts) -- triggers
  where
    rnd = lift (takeTMVar v) >>= w
    go (n, f) = (\a -> (n, a)) <$> R.mkHandler (`f` v)

mkDesign
    :: PC.Output (R.Disposable ())
    -> (Design specs -> R.ReactMlT STM ())
    -> [Trigger specs]
    -> [JE.Property]
    -> Many specs
    -> TMVar (Design specs) -- This must be empty!
    -> F R.Reactor (Design specs)
mkDesign dc w ts ps specs v = (\pln -> Design (pln, ps, specs)) <$> mkPlan dc w ts v

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
