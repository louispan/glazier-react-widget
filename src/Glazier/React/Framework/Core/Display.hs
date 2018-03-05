{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Display where

import Control.Lens
import Control.Monad.Reader
import qualified Data.DList as DL
import Data.Generics.Product
import Data.Maybe
import qualified Glazier.Core as Z
import qualified Glazier.React as Z
import qualified Glazier.React.Framework.Core.Model as Z
import qualified JavaScript.Extras as JE

type Display s m r = Z.Method s (Z.ReactMlT m) r

type FrameDisplay s m r = Display (Z.Frame s m) m r

-- | Gets the listeners for a particular 'GadgetId', usually for a
-- specific DOM element.
getListeners :: Z.GadgetId -> Z.Frame s m -> (DL.DList Z.Listener)
getListeners i s = fromMaybe [] (s ^. Z.plan.field @"listeners".at i)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
lf'
    :: Monad m
    => Z.GadgetId
    -> Z.Frame s m
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> Z.ReactMlT m ()
lf' i s = Z.leaf (getListeners i s)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
bh'
    :: Monad m
    => Z.GadgetId
    -> Z.Frame s m
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> Z.ReactMlT m a
    -> Z.ReactMlT m a
bh' i s = Z.branch (getListeners i s)


-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Core.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Core.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: (Z.MonadReactor m, Z.MonadScene v s t m) => t m ()
dirty = do
    (Z.Obj ref its) <- ask
    lift $ Z.doModifyIORef' ref (its.Z.plan.field @"currentFrameNum" %~ ((+ 1) . (`mod` JE.maxSafeInteger)))

rerender :: (Z.MonadReactor m, Z.MonadScene v s t m) => t m ()
rerender = do
    (Z.Obj ref its) <- ask
    lift $ do
        obj <- Z.doReadIORef ref
        let c = obj ^. its.Z.plan.field @"currentFrameNum"
            p = obj ^. its.Z.plan.field @"previousFrameNum"
        if c == p
            then pure ()
            else do
                Z.doModifyIORef' ref (its.Z.plan.field @"previousFrameNum" .~ c)
                Z.doSetComponentState
                    (JE.fromProperties [("frameNum", JE.toJSR c)])
                    (obj ^. (its.Z.plan.field @"component"))
