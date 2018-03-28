{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Scene where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.RWS
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Framework.MkId
import qualified JavaScript.Extras as JE

-- In order to remove effects we need to change to use State monad
-- where the state contains
-- ( the pure state s
-- , new triggers to activate: J.JSString, NOtice -> IO a, a -> b
-- , a way to retrieve io values back out of commands
-- )
-- and model needs to change so
-- component is map
-- react key is gadget key and is passed with Obj as the reader env
-- actually every is made into a map, so Plan is lifted into World = Map Plan


-- On the pure state processing side
-- create tmvar
-- define command that uses the tmvar
-- return command

-- This returns multiple commands split into multiple transactions
-- that can be run concurrently
-- a <- mkTMVar
-- b <- mkTMVar
-- cmda <- GetFilename (writeTMVar a)
-- cmdb <- GetUserName (writeTMVar b)
-- cmdc <- STMEffect $ do
--     obj <- ObjRef
--     do something with obj a + b
-- pure (new state, [cmda, cmdb, cmdc])
--
-- alternatively a single transaction run in serial.
-- let cmd = STMEffect $ (`runContT` pure) $ do
--   fn <- ContT $ doEffect GetFilename
--   un <- ContT $ doEffect GetUserName
--   do something with obj, fn and un
-- pure cmd
--
-- How to sugar conconurret version above so that you don't mkTMVar explicitly?
-- There is a difference in events and commands?
-- comands gets pushed to a separate queue (WriterT?)


-- So the engine (fired by trigger) now does:
-- read word IORef
-- run trigger state function and get (commands, new state)
-- update state ioref
-- check dirty and render
--
-- spawn multiple threads to asynchronously:
--   - run commands, retrying forever the ones that fail.
--   - command transactions should be small if possible


-- | Interactivity for a particular DOM element.
data Gizmo = Gizmo
    { targetRef :: Maybe EventTarget
    , listeners :: DL.DList Listener
    } deriving (G.Generic)

makeLenses_ ''Gizmo

instance CD.Dispose Gizmo where
    dispose (Gizmo _ ls) = foldMap (CD.dispose . snd) ls

newGizmo :: Gizmo
newGizmo = Gizmo Nothing mempty

data ShimListeners = ShimListeners
    -- render function of the ReactComponent
    { onRender :: J.Callback (IO J.JSVal)
    -- Run the on updated handlers below
    , onUpdated :: J.Callback (IO ())
    -- updates the componenRef
    , onRef :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeLenses_ ''ShimListeners

instance CD.Dispose ShimListeners where
    dispose (ShimListeners a b c) = CD.dispose a <> CD.dispose b <> CD.dispose c

-- | Interactivity data for a react component
data Plan = Plan
    -- Plan rquires planId for 'Glazier.React.Framework.Reactor.MkOnceOnUpdatedCallback'
    -- and 'Glazier.React.Framework.Reactor.MkEveryOnUpdatedCallback'
    { planId :: PlanId
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    , componentRef :: Maybe ComponentRef
    , shimListeners :: Maybe ShimListeners
    -- This is the previous "react state"
    , previousFrameNum :: Int
    -- This the current "react state".
    -- The glazier framework engine will use this to send a render request
    -- if the current didn't match previous.
    , currentFrameNum :: Int
      -- Things to dispose when this widget is removed
      -- cannot be hidden inside afterOnUpdated, as this also needs to be used
      -- when finalizing
    -- , disposeOnRemoved :: CD.Disposable
    --  Things to dispose on updated
    , disposeOnUpdated :: CD.Disposable
    -- interactivity data for child DOM elements
    , gizmos :: M.Map GizmoId Gizmo
    -- interactivity data for child react components
    , plans :: M.Map PlanId Plan
    } deriving (G.Generic)

makeLenses_ ''Plan

instance CD.Dispose Plan where
    dispose pln = fromMaybe mempty (CD.dispose <$> (shimListeners pln))
        <> (disposeOnUpdated pln)
        <> (foldMap CD.dispose (gizmos pln))
        <> (foldMap CD.dispose (plans pln))

newPlan :: PlanId -> Plan
newPlan i = Plan
    i
    Nothing
    Nothing
    0
    0
    -- mempty
    -- mempty
    -- mempty
    mempty
    mempty
    mempty


-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scene s =  Scene
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { scenePlan :: Plan
    , sceneModel :: s
    } deriving (G.Generic)

-- _model :: Lens (Scene s) (Scene s') s s'
-- _model = lens model (\s a -> s { model = a})

class HasModel t where
    _model :: Lens (t s) (t s') s s'

instance HasModel Scene where
    _model = lens sceneModel (\s a -> s { sceneModel = a})

-- _plan :: Lens' (Scene s) Plan
-- _plan = lens plan (\s a -> s { plan = a})

class HasPlan t where
    _plan :: Lens' t Plan

instance HasPlan (Scene s) where
    _plan = lens scenePlan (\s a -> s { scenePlan = a })

class HasScene t where
    _scene :: Lens (t s) (t s') (Scene s) (Scene s')

instance HasScene Scene where
    _scene = id

-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scenario c s = Scenario
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { commands :: DL.DList c
    , scene :: Scene s
    } deriving (G.Generic)

-- _commands :: Lens' (Scenario x s) (DL.DList x)
-- _commands = lens commands (\s a -> s { commands = a})

class HasCommands c t | t -> c where
    _commands :: Lens' t (DL.DList c)

instance HasCommands c (Scenario c s) where
    _commands = lens commands (\s a -> s { commands = a})

instance HasScene (Scenario c) where
    _scene = lens scene (\s a -> s { scene = a})

instance HasPlan (Scenario c s) where
    _plan = _scene._plan

instance HasModel (Scenario c) where
    _model = _scene._model

-- instance HasModel (Scenario x) where
--     _model = lens sceneModel (\s a -> s { sceneModel = a})

-- _scene :: Lens (Scenario x s) (Scenario x s') (Scene s) (Scene s')
-- _scene = lens scene (\s a -> s { scene = a})

editModel :: (Functor f, HasModel t) => LensLike' f s a -> LensLike' f (t s) (t a)
editModel l safa s = (\s' -> s & _model .~ s' ) <$> l afa' (s ^. _model)
  where
    afa' a = (view _model) <$> safa (s & _model .~ a)

editPlan :: (Functor f, HasPlan t) => LensLike' f Plan Plan -> LensLike' f t t
editPlan l safa s = (\s' -> s & _plan .~ s') <$> l afa' (s ^. _plan)
  where
    afa' a = (view _plan) <$> safa (s & _plan .~ a)

-- editSceneModel :: Functor f => LensLike' f s a -> LensLike' f (Scene s) (Scene a)
-- editSceneModel l safa s = (\s' -> s { model = s'} ) <$> l afa' (model s)
--   where
--     afa' a = model <$> safa (s { model = a})

-- editScenePlan :: Functor f => LensLike' f Plan Plan -> LensLike' f (Scene s) (Scene s)
-- editScenePlan l safa s = (\s' -> s { plan = s'} ) <$> l afa' (plan s)
--   where
--     afa' a = plan <$> safa (s { plan = a})

----------------------------------------------------------------------------------

-- type MonadWidget x s m = (MonadState (Scene x s) m, MonadWriter (DL.DList x) m)

-- -- type Initializer x s m = (Widget x s m, MonadCont m)

-- -- | Create a Plan and Gadget for this widget if it did not already exists
-- -- FIXME: naming
-- initialize :: MonadWidget x s m => m ()
-- initialize = do
--     (pid, gid) <- ask
--     -- first check if plan exists
--     _plans.at pid %= (Just . initGadget gid . fromMaybe newPlan)
--   where
--     initGadget gid = _gadgets.at gid %~ (Just . fromMaybe newGadget)

----------------------------------------------------------------------------------

-- | post a command to be interpreted at the end of the frame
post :: (HasCommands c s, MonadState s m) => DL.DList c -> m ()
post cs = _commands %= (<> cs)

post1 :: (HasCommands c s, MonadState s m) => c -> m ()
post1 c = _commands %= (`DL.snoc` c)

post1' :: (AsFacet c' c, HasCommands c s, MonadState s m) => c' -> m ()
post1' = post1 . review facet

----------------------------------------------------------------------------------

-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: (HasPlan s, MonadState s m) => m ()
dirty = _plan._currentFrameNum %= JE.safeModularIncrement

----------------------------------------------------------------------------------

editMyModel :: (HasModel t) => Traversal' s a -> ReifiedTraversal' w (t s) -> ReifiedTraversal' w (t a)
editMyModel l (Traversal t) = Traversal (t . (editModel l))
-- editMyModel :: Traversal' s a -> ReifiedTraversal' w (Scene s) -> ReifiedTraversal' w (Scene a)
-- editMyModel l (Traversal t) = Traversal (t . (editSceneModel l))

-- | Using a 'Traversal'' from a bigger plan to a contained plan,
-- convert a 'Traversal'' from the world to a bigger scene
-- to a 'Traversal'' from the world to the smaller scene.
editMyPlan :: (HasPlan t) => Traversal' Plan Plan -> ReifiedTraversal' w t -> ReifiedTraversal' w t
editMyPlan l (Traversal t) = Traversal (t . (editPlan l))

magnifyMyModel ::
    ( HasModel t
    , Magnify m n (ReifiedTraversal' w (t a)) (ReifiedTraversal' w (t s))
    , Contravariant (Magnified m c)
    )
    => Traversal' s a -> m c -> n c
magnifyMyModel l = magnify (to (editMyModel l))

magnifyMyPlan ::
    ( HasPlan t
    , Magnify m n (ReifiedTraversal' w t) (ReifiedTraversal' w t)
    , Contravariant (Magnified m c)
    )
    => Traversal' Plan Plan -> m c -> n c
magnifyMyPlan l = magnify (to (editMyPlan l))

magnifyModel ::
    ( HasModel t
    , Magnify m n (t a) (t s)
    , Functor (Magnified m c)
    )
    => LensLike' (Magnified m c) s a -> m c -> n c
magnifyModel l = magnify (editModel l)

magnifyPlan ::
    ( HasPlan t
    , Magnify m n t t
    , Functor (Magnified m c)
    )
    => LensLike' (Magnified m c) Plan Plan -> m c -> n c
magnifyPlan l = magnify (editPlan l)

--------------------------------------

-- class EnlargeModel c where
--     type WithEnlargedModel c s
--     type EnlargingModel c
--     enlargeModel :: Traversal' s (EnlargingModel c) -> c -> WithEnlargedModel c s

-- instance EnlargeModel ((->) (ReifiedTraversal' w (Scene x a)) r ) where
--     type WithEnlargedModel ((->) (ReifiedTraversal' w (Scene x a))r ) s = (->) (ReifiedTraversal' w (Scene x s)) r
--     type EnlargingModel ((->) (ReifiedTraversal' w (Scene x a))r ) = a
--     enlargeModel = enlargeMyModel

-- instance Monad m => EnlargeModel (ReadersT (ReifiedTraversal' w (Scene x a)) m r) where
--     type WithEnlargedModel (ReadersT (ReifiedTraversal' w (Scene x a)) m r) s = ReadersT (ReifiedTraversal' w (Scene x s)) m r
--     type EnlargingModel (ReadersT (ReifiedTraversal' w (Scene x a)) m r) = a
--     enlargeModel = enlargeMyModel

-- instance Monad m => EnlargeModel (ReaderT (ReifiedTraversal' w (Scene x a)) m r) where
--     type WithEnlargedModel (ReaderT (ReifiedTraversal' w (Scene x a)) m r) s = ReaderT (ReifiedTraversal' w (Scene x s)) m r
--     type EnlargingModel (ReaderT (ReifiedTraversal' w (Scene x a)) m r) = a
--     enlargeModel = enlargeMyModel

-- instance (Monoid u, Monad m) => EnlargeModel (RsS.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) where
--     type WithEnlargedModel (RsS.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) s = RsS.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r
--     type EnlargingModel (RsS.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) = a
--     enlargeModel = enlargeMyModel

-- instance (Monoid u, Monad m) => EnlargeModel (RsL.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) where
--     type WithEnlargedModel (RsL.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) s = RsL.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r
--     type EnlargingModel (RsL.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) = a
--     enlargeModel = enlargeMyModel

-- instance (Monoid u, Monad m) => EnlargeModel (RS.RWST (ReifiedTraversal' w (Scene x a)) u t m r) where
--     type WithEnlargedModel (RS.RWST (ReifiedTraversal' w (Scene x a)) u t m r) s = RS.RWST (ReifiedTraversal' w (Scene x s)) u t m r
--     type EnlargingModel (RS.RWST (ReifiedTraversal' w (Scene x a)) u t m r) = a
--     enlargeModel = enlargeMyModel

-- instance (Monoid u, Monad m) => EnlargeModel (RL.RWST (ReifiedTraversal' w (Scene x a)) u t m r) where
--     type WithEnlargedModel (RL.RWST (ReifiedTraversal' w (Scene x a)) u t m r) s = RL.RWST (ReifiedTraversal' w (Scene x s)) u t m r
--     type EnlargingModel (RL.RWST (ReifiedTraversal' w (Scene x a)) u t m r) = a
--     enlargeModel = enlargeMyModel

-- --------------------------------------

-- instance Monoid r => EnlargeModel ((->) (Scene x a) r) where
--     type WithEnlargedModel ((->) (Scene x a) r) s = (->) (Scene x s) r
--     type EnlargingModel ((->) (Scene x a) r) = a
--     enlargeModel = enlargeSceneModel

-- instance (Monoid r, Monad m) => EnlargeModel (ReadersT (Scene x a) m r) where
--     type WithEnlargedModel (ReadersT (Scene x a) m r) s = ReadersT (Scene x s) m r
--     type EnlargingModel (ReadersT (Scene x a) m r) = a
--     enlargeModel = enlargeSceneModel

-- instance (Monoid r, Monad m) => EnlargeModel (ReaderT (Scene x a) m r) where
--     type WithEnlargedModel (ReaderT (Scene x a) m r) s = ReaderT (Scene x s) m r
--     type EnlargingModel (ReaderT (Scene x a) m r) = a
--     enlargeModel = enlargeSceneModel

-- instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RsS.RWSsT (Scene x a) u t m r) where
--     type WithEnlargedModel (RsS.RWSsT (Scene x a) u t m r) s = RsS.RWSsT (Scene x s) u t m r
--     type EnlargingModel (RsS.RWSsT (Scene x a) u t m r) = a
--     enlargeModel = enlargeSceneModel

-- instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RsL.RWSsT (Scene x a) u t m r) where
--     type WithEnlargedModel (RsL.RWSsT (Scene x a) u t m r) s = RsL.RWSsT (Scene x s) u t m r
--     type EnlargingModel (RsL.RWSsT (Scene x a) u t m r) = a
--     enlargeModel = enlargeSceneModel

-- instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RS.RWST (Scene x a) u t m r) where
--     type WithEnlargedModel (RS.RWST (Scene x a) u t m r) s = RS.RWST (Scene x s) u t m r
--     type EnlargingModel (RS.RWST (Scene x a) u t m r) = a
--     enlargeModel = enlargeSceneModel

-- instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RL.RWST (Scene x a) u t m r) where
--     type WithEnlargedModel (RL.RWST (Scene x a) u t m r) s = RL.RWST (Scene x s) u t m r
--     type EnlargingModel (RL.RWST (Scene x a) u t m r) = a
--     enlargeModel = enlargeSceneModel

-- --------------------------------------

-- class EnlargePlan c where
--     enlargePlan :: Traversal' Plan Plan -> c -> c

-- instance EnlargePlan ((->) (ReifiedTraversal' w (Scene x s)) r) where
--     enlargePlan = enlargeMyPlan

-- instance Monad m => EnlargePlan (ReadersT (ReifiedTraversal' w (Scene x s)) m r) where
--     enlargePlan = enlargeMyPlan

-- instance Monad m => EnlargePlan (ReaderT (ReifiedTraversal' w (Scene x s)) m r) where
--     enlargePlan = enlargeMyPlan

-- instance (Monoid u, Monad m) => EnlargePlan (RsS.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r) where
--     enlargePlan = enlargeMyPlan

-- instance (Monoid u, Monad m) => EnlargePlan (RsL.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r) where
--     enlargePlan = enlargeMyPlan

-- instance (Monoid u, Monad m) => EnlargePlan (RS.RWST (ReifiedTraversal' w (Scene x s)) u t m r) where
--     enlargePlan = enlargeMyPlan

-- instance (Monoid u, Monad m)  => EnlargePlan (RL.RWST (ReifiedTraversal' w (Scene x s)) u t m r) where
--     enlargePlan = enlargeMyPlan

-- --------------------------------------

-- instance Monoid r => EnlargePlan ((->) (Scene x s) r) where
--     enlargePlan = enlargeScenePlan

-- instance (Monoid r, Monad m) => EnlargePlan (ReadersT (Scene x s) m r) where
--     enlargePlan = enlargeScenePlan

-- instance (Monoid r, Monad m) => EnlargePlan (ReaderT (Scene x s) m r) where
--     enlargePlan = enlargeScenePlan

-- instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RsS.RWSsT (Scene x s) u t m r) where
--     enlargePlan = enlargeScenePlan

-- instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RsL.RWSsT (Scene x s) u t m r) where
--     enlargePlan = enlargeScenePlan

-- instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RS.RWST (Scene x s) u t m r) where
--     enlargePlan = enlargeScenePlan

-- instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RL.RWST (Scene x s) u t m r) where
--     enlargePlan = enlargeScenePlan
