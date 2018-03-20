{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Glazier.React.Framework.Core.Model where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Readers
import qualified Control.Monad.Trans.RWS.Lazy as RL
import qualified Control.Monad.Trans.RWS.Strict as RS
import qualified Control.Monad.Trans.RWSs.Lazy as RsL
import qualified Control.Monad.Trans.RWSs.Strict as RsS
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Framework.Core.MkId


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
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    { componentRef :: Maybe ComponentRef
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
    -- FIXME: To be handled by the engine like Callbacks
    -- additional actions to take after every dirty
    -- , everyOnUpdated :: DL.DList x
    --  -- additional actions to take after a dirty
    -- , onceOnUpdated :: DL.DList x
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

newPlan :: Plan
newPlan = Plan
-- newPlan :: PlanId -> Plan
-- newPlan i = Plan
    -- i
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
data Scene x s =  Scene
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { commands :: DL.DList x
    , plan :: Plan
    , model :: s
    } deriving (G.Generic)

_commands :: Lens' (Scene x s) (DL.DList x)
_commands = lens commands (\s a -> s { commands = a})

_plan :: Lens' (Scene x s) Plan
_plan = lens plan (\s a -> s { plan = a})

_model :: Lens (Scene x s) (Scene x s') s s'
_model = lens model (\s a -> s { model = a})

-- class HasScene c x s | c -> x s where
--     _scene :: Lens' c (Scene x s)

-- instance HasScene (Scene x s) x s where
--     _scene = id

-- data Frame x w =  Frame
--     -- commands could be in a writer monad, but then you can't get
--     -- a MonadWriter with ContT, but you can have a MonadState with ContT.
--     { commands :: DL.DList x
--     , world :: w
--     } deriving (G.Generic)

-- _world :: Lens' (Frame x s) s
-- _world = lens world (\s a -> s { world = a})

-- editWorld :: Lens' s' s -> Lens' (Scene x s') (Scene x s)
-- editWorld l = lens
--     (\s' -> s' & _model %~ (view l))
--     (\s' a -> a & _model %~ (\s -> (model s') & l .~ s))


editSceneModel :: Functor f => LensLike' f s a -> LensLike' f (Scene x s) (Scene x a)
editSceneModel l safa s = (\s' -> s { model = s'} ) <$> l afa' (model s)
  where
    afa' a = model <$> safa (s { model = a})

editScenePlan :: Functor f => LensLike' f Plan Plan -> LensLike' f (Scene x s) (Scene x s)
editScenePlan l safa s = (\s' -> s { plan = s'} ) <$> l afa' (plan s)
  where
    afa' a = plan <$> safa (s { plan = a})

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
post :: (MonadState (Scene x s) m) => DL.DList x -> m ()
post xs = _commands %= (<> xs)

post1 :: (MonadState (Scene x s) m) => x -> m ()
post1 x = _commands %= (`DL.snoc` x)

post1' :: (AsFacet c x, MonadState (Scene x s) m) => c -> m ()
post1' = post1 . review facet



-- -- Add an action to run once after the next render
-- addOnceOnUpdated :: MonadState (Scene x s) m => x -> m ()
-- addOnceOnUpdated x = _plan._onceOnUpdated %= (`DL.snoc` x)

-- -- Add an action to run after every render
-- addEveryOnUpdated :: MonadState (Scene x s) m => x -> m ()
-- addEveryOnUpdated x = _plan._everyOnUpdated %= (`DL.snoc` x)


----------------------------------------------------------------------------------

class EnlargeModel c where
    type WithEnlargedModel c s
    type EnlargingModel c
    enlargeModel :: Traversal' s (EnlargingModel c) -> c -> WithEnlargedModel c s

class EnlargePlan c where
    enlargePlan :: Traversal' Plan Plan -> c -> c

-- | Magnfiy the reader environment of 'MethodT'
editMyModel :: Traversal' s a -> ReifiedTraversal' w (Scene x s) -> ReifiedTraversal' w (Scene x a)
editMyModel l (Traversal t) = Traversal (t . (editSceneModel l))

-- | Using a 'Traversal'' from a bigger plan to a contained plan,
-- convert a 'Traversal'' from the world to a bigger scene
-- to a 'Traversal'' from the world to the smaller scene.
editMyPlan :: Traversal' Plan Plan -> ReifiedTraversal' w (Scene x s) -> ReifiedTraversal' w (Scene x s)
editMyPlan l (Traversal t) = Traversal (t . (editScenePlan l))

instance EnlargeModel ((->) (ReifiedTraversal' w (Scene x a)) r ) where
    type WithEnlargedModel ((->) (ReifiedTraversal' w (Scene x a))r ) s = (->) (ReifiedTraversal' w (Scene x s)) r
    type EnlargingModel ((->) (ReifiedTraversal' w (Scene x a))r ) = a
    enlargeModel l = magnify (to (editMyModel l))

instance Monad m => EnlargeModel (ReadersT (ReifiedTraversal' w (Scene x a)) m r) where
    type WithEnlargedModel (ReadersT (ReifiedTraversal' w (Scene x a)) m r) s = ReadersT (ReifiedTraversal' w (Scene x s)) m r
    type EnlargingModel (ReadersT (ReifiedTraversal' w (Scene x a)) m r) = a
    enlargeModel l = magnify (to (editMyModel l))

instance Monad m => EnlargeModel (ReaderT (ReifiedTraversal' w (Scene x a)) m r) where
    type WithEnlargedModel (ReaderT (ReifiedTraversal' w (Scene x a)) m r) s = ReaderT (ReifiedTraversal' w (Scene x s)) m r
    type EnlargingModel (ReaderT (ReifiedTraversal' w (Scene x a)) m r) = a
    enlargeModel l = magnify (to (editMyModel l))

instance (Monoid u, Monad m) => EnlargeModel (RsS.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) where
    type WithEnlargedModel (RsS.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) s = RsS.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r
    type EnlargingModel (RsS.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) = a
    enlargeModel l = magnify (to (editMyModel l))

instance (Monoid u, Monad m) => EnlargeModel (RsL.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) where
    type WithEnlargedModel (RsL.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) s = RsL.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r
    type EnlargingModel (RsL.RWSsT (ReifiedTraversal' w (Scene x a)) u t m r) = a
    enlargeModel l = magnify (to (editMyModel l))

instance (Monoid u, Monad m) => EnlargeModel (RS.RWST (ReifiedTraversal' w (Scene x a)) u t m r) where
    type WithEnlargedModel (RS.RWST (ReifiedTraversal' w (Scene x a)) u t m r) s = RS.RWST (ReifiedTraversal' w (Scene x s)) u t m r
    type EnlargingModel (RS.RWST (ReifiedTraversal' w (Scene x a)) u t m r) = a
    enlargeModel l = magnify (to (editMyModel l))

instance (Monoid u, Monad m) => EnlargeModel (RL.RWST (ReifiedTraversal' w (Scene x a)) u t m r) where
    type WithEnlargedModel (RL.RWST (ReifiedTraversal' w (Scene x a)) u t m r) s = RL.RWST (ReifiedTraversal' w (Scene x s)) u t m r
    type EnlargingModel (RL.RWST (ReifiedTraversal' w (Scene x a)) u t m r) = a
    enlargeModel l = magnify (to (editMyModel l))

--------------------------------------

instance Monoid r => EnlargeModel ((->) (Scene x a) r) where
    type WithEnlargedModel ((->) (Scene x a) r) s = (->) (Scene x s) r
    type EnlargingModel ((->) (Scene x a) r) = a
    enlargeModel l = magnify (editSceneModel l)

instance (Monoid r, Monad m) => EnlargeModel (ReadersT (Scene x a) m r) where
    type WithEnlargedModel (ReadersT (Scene x a) m r) s = ReadersT (Scene x s) m r
    type EnlargingModel (ReadersT (Scene x a) m r) = a
    enlargeModel l = magnify (editSceneModel l)

instance (Monoid r, Monad m) => EnlargeModel (ReaderT (Scene x a) m r) where
    type WithEnlargedModel (ReaderT (Scene x a) m r) s = ReaderT (Scene x s) m r
    type EnlargingModel (ReaderT (Scene x a) m r) = a
    enlargeModel l = magnify (editSceneModel l)

instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RsS.RWSsT (Scene x a) u t m r) where
    type WithEnlargedModel (RsS.RWSsT (Scene x a) u t m r) s = RsS.RWSsT (Scene x s) u t m r
    type EnlargingModel (RsS.RWSsT (Scene x a) u t m r) = a
    enlargeModel l = magnify (editSceneModel l)

instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RsL.RWSsT (Scene x a) u t m r) where
    type WithEnlargedModel (RsL.RWSsT (Scene x a) u t m r) s = RsL.RWSsT (Scene x s) u t m r
    type EnlargingModel (RsL.RWSsT (Scene x a) u t m r) = a
    enlargeModel l = magnify (editSceneModel l)

instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RS.RWST (Scene x a) u t m r) where
    type WithEnlargedModel (RS.RWST (Scene x a) u t m r) s = RS.RWST (Scene x s) u t m r
    type EnlargingModel (RS.RWST (Scene x a) u t m r) = a
    enlargeModel l = magnify (editSceneModel l)

instance (Monoid r, Monoid u, Monad m) => EnlargeModel (RL.RWST (Scene x a) u t m r) where
    type WithEnlargedModel (RL.RWST (Scene x a) u t m r) s = RL.RWST (Scene x s) u t m r
    type EnlargingModel (RL.RWST (Scene x a) u t m r) = a
    enlargeModel l = magnify (editSceneModel l)

--------------------------------------

instance EnlargePlan ((->) (ReifiedTraversal' w (Scene x s)) r) where
    enlargePlan l = magnify (to (editMyPlan l))

instance Monad m => EnlargePlan (ReadersT (ReifiedTraversal' w (Scene x s)) m r) where
    enlargePlan l = magnify (to (editMyPlan l))

instance Monad m => EnlargePlan (ReaderT (ReifiedTraversal' w (Scene x s)) m r) where
    enlargePlan l = magnify (to (editMyPlan l))

instance (Monoid u, Monad m) => EnlargePlan (RsS.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r) where
    enlargePlan l = magnify (to (editMyPlan l))

instance (Monoid u, Monad m) => EnlargePlan (RsL.RWSsT (ReifiedTraversal' w (Scene x s)) u t m r) where
    enlargePlan l = magnify (to (editMyPlan l))

instance (Monoid u, Monad m) => EnlargePlan (RS.RWST (ReifiedTraversal' w (Scene x s)) u t m r) where
    enlargePlan l = magnify (to (editMyPlan l))

instance (Monoid u, Monad m)  => EnlargePlan (RL.RWST (ReifiedTraversal' w (Scene x s)) u t m r) where
    enlargePlan l = magnify (to (editMyPlan l))

--------------------------------------

instance Monoid r => EnlargePlan ((->) (Scene x s) r) where
    enlargePlan l = magnify (editScenePlan l)

instance (Monoid r, Monad m) => EnlargePlan (ReadersT (Scene x s) m r) where
    enlargePlan l = magnify (editScenePlan l)

instance (Monoid r, Monad m) => EnlargePlan (ReaderT (Scene x s) m r) where
    enlargePlan l = magnify (editScenePlan l)

instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RsS.RWSsT (Scene x s) u t m r) where
    enlargePlan l = magnify (editScenePlan l)

instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RsL.RWSsT (Scene x s) u t m r) where
    enlargePlan l = magnify (editScenePlan l)

instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RS.RWST (Scene x s) u t m r) where
    enlargePlan l = magnify (editScenePlan l)

instance (Monoid r, Monoid u, Monad m) => EnlargePlan (RL.RWST (Scene x s) u t m r) where
    enlargePlan l = magnify (editScenePlan l)
