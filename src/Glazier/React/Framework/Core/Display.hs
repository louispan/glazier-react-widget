{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Display where

import Control.Lens
import Control.Monad.State
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React
import Glazier.React.Framework.Core.Model
import qualified JavaScript.Extras as JE

-- type Display x s m r = Widget x s m => ReactMlT m r

-- type SceneDisplay x s r = Display (Scene x s) r
----------------------------------------------------------------------------------

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
lf' :: MonadState (Scene x s) m
    => GadgetId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> (DL.DList JE.Property)
    -> ReactMlT m ()
lf' gid n props = do
    ls <- lift $ use (_plan._gadgets.ix gid._listeners)
    leaf ls n props

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
bh' :: MonadState (Scene x s) m
    => GadgetId
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> ReactMlT m a
    -> ReactMlT m a
bh' gid n props childs = do
    ls <- lift $ use (_plan._gadgets.ix gid._listeners)
    branch ls n props childs


-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Core.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Core.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: MonadState (Scene x s) m => m ()
dirty = _plan._currentFrameNum %= ((+ 1) . (`mod` JE.maxSafeInteger))

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
