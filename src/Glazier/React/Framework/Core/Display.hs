{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Display where

import Control.Lens
import qualified Data.DList as DL
import Data.Generics.Product
import Data.Maybe
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Model as R
import qualified Glazier.React.Framework.Core.Obj as R
import qualified JavaScript.Extras as JE

type Display m s r = s -> R.ReactMlT m r

type FrameDisplay m s r = Display m (R.Frame m s) r

-- | Gets the listeners for a particular 'GadgetId', usually for a
-- specific DOM element.
getListeners :: R.GadgetId -> R.Frame m s -> (DL.DList R.Listener)
getListeners i s = fromMaybe [] (s ^. R.plan.field @"listeners".at i)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
lf'
    :: Monad m
    => R.GadgetId
    -> R.Frame m s
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> R.ReactMlT m ()
lf' i s = R.leaf (getListeners i s)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
bh'
    :: Monad m
    => R.GadgetId
    -> R.Frame m s
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> R.ReactMlT m a
    -> R.ReactMlT m a
bh' i s = R.branch (getListeners i s)


-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Core.Trigger.trigger'
-- This means calling 'stale' on other widgets from a different widget's 'Glazier.React.Framework.Core.Trigger.trigger'
-- will not result in a rerender for the other widget.
stale :: R.MonadReactor m => R.Scene m v s -> m ()
stale (R.Obj ref its) =
    R.doModifyIORef' ref (its.R.plan.field @"currentFrameNum" %~ ((+ 1) . (`mod` JE.maxSafeInteger)))

rerender :: R.MonadReactor m => R.Scene m v s -> m ()
rerender (R.Obj ref its) = do
    obj <- R.doReadIORef ref
    let c = obj ^. its.R.plan.field @"currentFrameNum"
        p = obj ^. its.R.plan.field @"previousFrameNum"
    if c == p
        then pure ()
        else do
            R.doModifyIORef' ref (its.R.plan.field @"previousFrameNum" .~ c)
            R.doSetComponentState
                (JE.fromProperties [("frameNum", JE.toJSR c)])
                (obj ^. (its.R.plan.field @"component"))
