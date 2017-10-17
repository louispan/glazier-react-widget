{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE UndecidableInstances #-}

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

import Control.Applicative
import Control.DeepSeq
-- import Control.Lens
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.IORef
-- import qualified Data.JSString as JS
import Data.Kind
import Data.Semigroup
-- import qualified GHC.Generics as G
-- import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
-- import qualified Glazier.React.Commands.Rerender as C
-- import qualified JavaScript.Extras as JE

----------------------------------------------------------

usingIORef :: R.MonadReactor m => IORef s -> MaybeT (StateT s m) a -> MaybeT m a
usingIORef this m = do
    s <- lift $ R.doReadIORef this
    (a, s') <- lift $ runStateT (runMaybeT m) s
    case a of
        Nothing -> empty
        Just a' -> do
            lift $ R.doWriteIORef this s'
            pure a'

catMaybeT :: (Applicative m, Semigroup a) => MaybeT m a -> MaybeT m a -> MaybeT m a
catMaybeT (MaybeT m) (MaybeT m') = MaybeT (liftA2 (<>) m m')

----------------------------------------------------------

-- class HasProperties c where
--     properties :: Lens' c [JE.Property]

-- instance HasProperties [JE.Property] where
--     properties = id

-- type Properties = [JE.Property]

----------------------------------------------------------

-- | Plan has to be stored differently to other plans because mkPlan needs
-- additional parameters

newtype Key = Key { runKey :: J.JSString } deriving R.Dispose
newtype FrameNum = FrameNum { runFrameNum :: Int } deriving R.Dispose

class ModelWrapper (w :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) where
    wrapModel :: (s -> t) -> (t -> s) -> w m s -> w m t
    wrapMModel :: (s -> m t) -> (t -> m s) -> w m s -> w m t

class AModelWrapper a (w :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) s | a -> w m s where
    toModelWrapper :: a -> w m s
    fromModelWrapper :: w m s -> a

wrapModel'
    :: (AModelWrapper a w m s, AModelWrapper b w m t, ModelWrapper w m)
    => (s -> t) -> (t -> s) -> a -> b
wrapModel' f g = fromModelWrapper . wrapModel f g . toModelWrapper

wrapMModel'
    :: (AModelWrapper a w m s, AModelWrapper b w m t, ModelWrapper w m)
    => (s -> m t) -> (t -> m s) -> a -> b
wrapMModel' f g = fromModelWrapper . wrapMModel f g . toModelWrapper

class PlanWrapper (w :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) where
    wrapPlan :: (p -> q) -> (q -> p) -> w m p -> w m q
    wrapMPlan :: (p -> m q) -> (q -> m p) -> w m p -> w m q

class APlanWrapper a (w :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) p | a -> w m p where
    toPlanWrapper :: a -> w m p
    fromPlanWrapper :: w m p -> a

wrapPlan'
    :: (APlanWrapper a w m p, APlanWrapper b w m q, PlanWrapper w m)
    => (p -> q) -> (q -> p) -> a -> b
wrapPlan' f g = fromPlanWrapper . wrapPlan f g . toPlanWrapper

wrapMPlan'
    :: (APlanWrapper a w m p, APlanWrapper b w m q, PlanWrapper w m)
    => (p -> m q) -> (q -> m p) -> a -> b
wrapMPlan' f g = fromPlanWrapper . wrapMPlan f g . toPlanWrapper

-- class WrapRequirements x r' where
--     type WrappedRequirements x s'
--     wrapRequirements :: (Many r -> r') -> (r' -> Many r) -> x -> WrappedRequirements x r'
--     wrapRequirementsM :: (Many r -> m r') -> (r' -> m (Many r)) -> x -> WrappedRequirements x r'

-- data ComponentPlan = ComponentPlan
--     { _component :: R.ReactComponent
--     , _onRender ::  J.Callback (IO J.JSVal)
--     , _onComponentRef :: J.Callback (J.JSVal -> IO ())
--     , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
--     } deriving (G.Generic)

-- makeClassy ''ComponentPlan

-- instance R.Dispose ComponentPlan

-- data Plan = Plan
--     { _frameNum :: FrameNum
--     , _componentRef :: C.ComponentRef
--     , _deferredDisposables :: R.Disposable ()
--     , _listeners :: [R.Listener]
--     , _key :: J.JSString
--     , _componentPlan' :: Maybe ComponentPlan
--     } deriving (G.Generic)

-- makeClassy ''Plan

-- instance R.Dispose Plan

----------------------------------------------------------

-- class HasSpecifications c specs | c -> specs where
--     specifications :: Lens' c (Many specs)

----------------------------------------------------------

-- newtype Design (specs :: [Type]) = Design
--     { runDesign ::
--         ( [JE.Property]
--         , Many specs
--         -- , Plan
--         )
--     }
--     deriving G.Generic

-- -- | UndecidableInstances, but safe because @Many specs@ is smaller than @Design specs@
-- instance (R.Dispose (Many specs)) => R.Dispose (Design specs)

-- instance HasPlan (Design specs) where
--     plan = _Design . _3

-- instance HasProperties (Design specs) where
--     properties = _Design . _1

-- instance HasSpecifications (Design specs) specs where
--     specifications = _Design . _2

-- _Design :: Iso
--     (Design specs)
--     (Design specs')
--     ([JE.Property], Many specs, Plan)
--     ([JE.Property], Many specs', Plan)
-- _Design = iso runDesign Design

----------------------------------------------------------

-- rerender :: Monad m => MaybeT (StateT (Design specs) m) C.Rerender
-- rerender = do
--     -- Just change the state to a different number so the React PureComponent will call render()
--     (plan . frameNum) %= (\(FrameNum i) -> FrameNum $ (i `mod` JE.maxSafeInteger) + 1)
--     FrameNum i <- use (plan . frameNum)
--     r <- use (plan . componentRef)
--     pure $ C.Rerender r [("frameNum", JE.JSVar $ JE.toJS i)]

----------------------------------------------------------

-- queueDisposable :: (Monad m, R.Dispose a) => a -> MaybeT (StateT (Design specs) m) ()
-- queueDisposable a = (plan . deferredDisposables) %= (>> R.dispose a)

-- inactivePlan
--     :: Plan
-- inactivePlan = Plan
--     (FrameNum 0) -- frameNum
--     (C.ComponentRef J.nullRef) -- componentRef
--     mempty -- deferredDisposables
--     mempty -- traverse go ts -- triggers
--     mempty -- R.mkKey' -- key
--     Nothing -- componentPlan

-- mkComponentPlan
--     :: R.MonadReactor m
--     => (Design specs -> R.ReactMlT m ())
--     -> IORef (Design specs)
--     -> m (ComponentPlan)
-- mkComponentPlan w this = ComponentPlan
--     <$> R.getComponent -- component
--     <*> R.mkRenderer rnd -- onRender
--     <*> (R.mkCallback (pure . \j -> [j]) (usingIORef this . zoom plan . doOnComponentRef) pure) -- onComponentRef
--     <*> (R.mkCallback (pure . const [()]) (
--             usingIORef this
--             . zoom plan
--             . const doOnComponentDidUpdate) (lift . R.runDisposable)) --onComopnentDidUpdate
--   where
--     rnd = lift (R.doReadIORef this) >>= w

--     doOnComponentRef :: Monad m => J.JSVal -> MaybeT (StateT Plan m) [()]
--     doOnComponentRef j = do
--         componentRef .= C.ComponentRef j
--         pure []

--     doOnComponentDidUpdate :: Monad m => MaybeT (StateT Plan m) [R.Disposable ()]
--     doOnComponentDidUpdate = do
--         -- Run delayed commands that need to wait until frame is re-rendered
--         -- Eg focusing after other rendering changes
--         ds <- use deferredDisposables
--         deferredDisposables .= mempty
--         pure [ds]

-- initPlan
--     :: R.MonadReactor m
--     => (Design specs -> R.ReactMlT m ())
--     -> IORef (Design specs)
--     -> Plan
--     -> m (Maybe Plan)
-- initPlan _ _ (Plan _ _ _ _ _ (Just _)) = pure Nothing
-- initPlan w this (Plan frm cRef defDisp ls k Nothing) = fmap Just $ Plan frm cRef defDisp ls
--     <$> ((k `JS.append`) <$> R.mkKey') -- key
--     <*> (Just <$> mkComponentPlan w this)

mkListeners
    :: (R.MonadReactor m, NFData (Which a))
    => (Which cmds -> MaybeT IO ())
    -> (Which a -> MaybeT m (DL.DList (Which cmds)))
    -> [(J.JSString, J.JSVal -> MaybeT IO (DL.DList (Which a)))]
    -> m [R.Listener]
mkListeners exec hdl = traverse toListener -- triggers
  where
    toListener (n, f) = (\a -> (n, a)) <$> R.mkCallback f hdl exec

-- inactiveDesign
--     :: [JE.Property]
--     -> Many specs
--     -> Design specs
-- inactiveDesign ps specs = Design (ps, specs, inactivePlan)

-- initDesign
--     :: R.MonadReactor m => (Design specs -> R.ReactMlT m ())
--     -> IORef (Design specs)
--     -> MaybeT m (Design specs)
-- initDesign w this = do
--     obj <- lift $ R.doReadIORef this
--     pln <- MaybeT $ initPlan w this (obj ^. plan)
--     pure (obj & plan .~ pln)

-- componentWindow :: Monad m => Design specs -> R.ReactMlT m ()
-- componentWindow s =
--     let cPlan = s ^. plan . componentPlan'
--     in case cPlan of
--         Nothing -> pure ()
--         Just cPlan' -> R.lf
--            (cPlan' ^. component . to JE.toJS')
--             [ ("ref", cPlan' ^. onComponentRef)
--             , ("componentDidUpdate", cPlan' ^. onComponentDidUpdate)
--             ]
--             [ ("key", s ^. plan . key . to JE.toJS')
--             -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
--             , ("render", cPlan' ^. onRender . to JE.toJS')
--             ]
