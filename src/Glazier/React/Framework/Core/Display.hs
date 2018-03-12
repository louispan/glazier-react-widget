{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Display where

import Control.Lens
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React
import Glazier.React.Framework.Core.Model
import qualified JavaScript.Extras as JE

-- type Display x s m r = Widget x s m => ReactMlT m r

-- type SceneDisplay x s r = Display (Scene x s) r

-- | Gets the listeners for a particular 'GadgetId', usually for a
-- specific DOM element.
myListeners :: Widget x s m => m (DL.DList Listener)
myListeners = do
    Traversal _gad <- myGadget
    use (_gad._listeners)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
lf' :: Widget x s m
    => JE.JSRep -- ^ eg "div" or "input"
    -> (DL.DList JE.Property)
    -> ReactMlT m ()
lf' n props = do
    ls <- lift myListeners
    leaf ls n props

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
bh' :: Widget x s m
    => JE.JSRep
    -> (DL.DList JE.Property)
    -> ReactMlT m a
    -> ReactMlT m a
bh' n props childs = do
    ls <- lift myListeners
    branch ls n props childs

-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Core.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Core.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: Widget x s m => m ()
dirty = do
    pid <- view _planId
    _plans.ix pid._currentFrameNum %= ((+ 1) . (`mod` JE.maxSafeInteger))

--     (JE.fromProperties [("frameNum", JE.toJSR c)])
data Rerender = Rerender ReactComponent Int

rerender :: (AsFacet Rerender x, Widget x s m) => m ()
rerender = do
    Traversal _pln <- myPlan
    void $ runMaybeT $ do
        pln <- MaybeT . preuse $ _pln
        let c = pln ^. _currentFrameNum
            p = pln ^. _previousFrameNum
            comp = pln ^. _component
        if (c == p)
            then pure ()
            else do
                _pln._previousFrameNum .= c
                tell [review facet $ Rerender comp c]
