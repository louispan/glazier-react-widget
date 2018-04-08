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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Scene where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tagged
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Framework.MkId
import qualified JavaScript.Extras as JE

----------------------------------------------------------------------------------

-- | Interactivity for a particular DOM element.
-- type Listener = (J.JSString, J.Callback (J.JSVal -> IO ()))
data Gizmo = Gizmo
    { targetRef :: Maybe EventTarget
    -- (name of event, context of event)
    , listeners :: M.Map J.JSString (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Every" (JE.JSRep -> IO ()))
    } deriving (G.Generic)

makeLenses_ ''Gizmo

newGizmo :: Gizmo
newGizmo = Gizmo Nothing mempty

----------------------------------------------------------------------------------

data ShimCallbacks = ShimCallbacks
    -- render function of the ReactComponent
    { onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    -- Run the doOnUpdated in the plan
    , onUpdated :: J.Callback (IO ())
    -- updates the componenRef
    , onRef :: J.Callback (J.JSVal -> IO ())
    -- all listeners use the same entry function, just a different
    -- first arg context.
    , onListen :: J.Callback (J.JSVal -> J.JSVal -> IO ())
    } deriving (G.Generic)

makeLenses_ ''ShimCallbacks

instance CD.Dispose ShimCallbacks where
    dispose (ShimCallbacks a b c d) = CD.dispose a <> CD.dispose b <> CD.dispose c <> CD.dispose d

----------------------------------------------------------------------------------

-- | Interactivity data for a react component
data Plan = Plan
    -- Plan rquires planId for 'Glazier.React.Framework.Reactor.MkOnceOnUpdatedCallback'
    -- and 'Glazier.React.Framework.Reactor.MkEveryOnUpdatedCallback'
    -- { planId :: PlanId
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    { componentRef :: Maybe ComponentRef
    , shimCallbacks :: Maybe ShimCallbacks
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
    , doOnUpdated :: (Tagged "Once" (IO ()), Tagged "Every" (IO ()))
    --  Things to dispose on updated
    , disposeOnUpdated :: CD.Disposable
    -- interactivity data for child DOM elements
    , gizmos :: M.Map GizmoId Gizmo
    -- interactivity data for child react components
    , plans :: M.Map PlanId (TVar Plan)
    } deriving (G.Generic)

makeLenses_ ''Plan

instance CD.Dispose Plan where
    dispose pln = fromMaybe mempty (CD.dispose <$> (shimCallbacks pln))
        <> (disposeOnUpdated pln)
        <> (foldMap CD.dispose (plans pln))

newPlan :: Plan
newPlan = Plan
    Nothing
    Nothing
    0
    0
    (Tagged mempty, Tagged mempty)
    mempty
    mempty
    mempty

----------------------------------------------------------------------------------

-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scene s = Scene
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { plan :: Plan
    , model :: s
    } deriving (G.Generic)

_model :: Lens (Scene s) (Scene s') s s'
_model = lens model (\s a -> s { model = a})

_plan :: Lens' (Scene s) Plan
_plan = lens plan (\s a -> s { plan = a})

----------------------------------------------------------------------------------

-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scenario c s = Scenario
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { commands :: DL.DList c
    , scene :: Scene s
    } deriving (G.Generic)

_commands :: Lens' (Scenario c s) (DL.DList c)
_commands = lens commands (\s a -> s { commands = a})

_scene :: Lens (Scenario x s) (Scenario x s') (Scene s) (Scene s')
_scene = lens scene (\s a -> s { scene = a})

editSceneModel :: (Functor f) => LensLike' f s a -> LensLike' f (Scene s) (Scene a)
editSceneModel l safa s = (\s' -> s & _model .~ s' ) <$> l afa' (s ^. _model)
  where
    afa' a = (view _model) <$> safa (s & _model .~ a)

editScenarioModel :: (Functor f) => LensLike' f s a -> LensLike' f (Scenario c s) (Scenario c a)
editScenarioModel l safa s = (\s' -> s & _scene._model .~ s' ) <$> l afa' (s ^. _scene._model)
  where
    afa' a = (view (_scene._model)) <$> safa (s & _scene._model .~ a)


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

-- -- | 'post' . 'cmd'
-- postCmd :: (AsFacet c' c, MonadState (Scenario c s) m) => c' -> m ()
-- postCmd = post . cmd

-- -- | A variation of 'post1' for commands with a type variable @c@,
-- -- which should be commands that require a continuation.
-- -- Ie. commands that "return" a value from running an effect.
-- postCmd' :: (AsFacet (c' c) c, MonadState (Scenario c s) m) => c' c -> m ()
-- postCmd' = post . cmd'

-- Add a command to the list of commands for this state tick.
post :: (MonadState (Scenario c s) m) => c -> m ()
post c = _commands %= (`DL.snoc` c)

-- | convert a request type to a command type.
-- This is used for commands that doesn't have a continuation.
-- Ie. commands that doesn't "returns" a value from running an effect.
-- Use 'cmd'' for commands that require a continuation ("returns" a value).
cmd :: (AsFacet c' c) => c' -> c
cmd = review facet

-- | A variation of 'cmd' for commands with a type variable @c@,
-- which is usually commands that are containers of command,
-- or commands that require a continuation
-- Eg. commands that "returns" a value from running an effect.
-- 'cmd'' is usually used with with the 'Cont' monad to help
-- create the continuation.
--
-- @
-- post $ (`runCont` id) $ do
--     a <- cont $ cmd' . GetSomething
--     pure . cmd $ DoSomething (f a)
-- @
cmd' :: (AsFacet (c' c) c) => c' c -> c
cmd' = cmd

-- | runs a MaybeT over a monad that creates a command.
runMaybeCmd :: (AsFacet [c] c, Monad m) => MaybeT m c -> m c
runMaybeCmd m = runMaybeT m >>= maybe (pure $ cmd' @[] []) pure

-- ccmd :: AsFacet (c' c) c => ((a -> c) -> c' c) -> Cont c a
-- ccmd k = cont $ cmd' . k

----------------------------------------------------------------------------------

data SceneObj p s = SceneObj
    { planVar ::TVar Plan
    , modelVar ::TVar p
    , self :: Traversal' p s
    }

access :: Traversal' s a -> SceneObj p s -> SceneObj p a
access l (SceneObj pln mdl s) = SceneObj pln mdl (s.l)

magnifyObjModel ::
    ( Magnify m n (SceneObj p a) (SceneObj p b)
    , Contravariant (Magnified m r)
    )
    => Traversal' b a -> m r -> n r
magnifyObjModel l = magnify (to (access l))

magnifyModel ::
    ( Magnify m n (Scene a) (Scene b)
    , Functor (Magnified m r)
    )
    => LensLike' (Magnified m r) b a -> m r -> n r
magnifyModel l = magnify (editSceneModel l)
