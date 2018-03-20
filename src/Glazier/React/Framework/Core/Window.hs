{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Window where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.RWSs.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React
import Glazier.React.Framework.Core.MkId
import Glazier.React.Framework.Core.Model
import qualified JavaScript.Extras as JE

type WindowT x s m = RWSsT (Scene x s) () (DL.DList ReactMarkup) m

type Window x s = WindowT x s Identity

-- type SceneDisplay x s r = Display (Scene x s) r
----------------------------------------------------------------------------------

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for a 'GizmoId'.
lf' :: Monad m
    => GizmoId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> (DL.DList JE.Property)
    -> WindowT x s m ()
lf' gid n props = do
    ls <- view (_plan._gizmos.ix gid._listeners)
    leaf ls n props

-- | Interactive version of 'bh'
bh' :: Monad m
    => GizmoId
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> WindowT x s m r
    -> WindowT x s m r
bh' gid n props childs = do
    ls <- view (_plan._gizmos.ix gid._listeners)
    branch ls n props childs

-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Core.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Core.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: MonadState (Scene x s) m => m ()
dirty = _plan._currentFrameNum %= JE.safeModularIncrement

--     (JE.fromProperties [("frameNum", JE.toJSR c)])
data Rerender = Rerender ComponentRef Int

rerender :: (AsFacet Rerender x, MonadState (Scene x s) m) => m ()
rerender = do
    c <- use (_plan._currentFrameNum)
    p <- use (_plan._previousFrameNum)
    comp <- use (_plan._componentRef)
    case (c /= p, comp) of
        (True, Just comp') -> do
            _plan._previousFrameNum .= c
            post1' $ Rerender comp' c
        _ -> pure ()
