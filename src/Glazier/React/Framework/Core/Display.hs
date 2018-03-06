{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Display where

import Control.Lens
import qualified Data.DList as DL
import Data.Maybe
import Glazier.Core
import Glazier.React
import Glazier.React.Framework.Core.Model
import qualified JavaScript.Extras as JE

type Display s m r = Method s (ReactMlT m) r

type FrameDisplay s m r = Display (Frame m s) m r

-- | Gets the listeners for a particular 'GadgetId', usually for a
-- specific DOM element.
getListeners :: GadgetId -> Frame m s -> (DL.DList Listener)
getListeners i s = fromMaybe [] (s ^. _plan._listeners.at i)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
lf'
    :: Monad m
    => GadgetId
    -> Frame m s
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> ReactMlT m ()
lf' i s = leaf (getListeners i s)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
bh'
    :: Monad m
    => GadgetId
    -> Frame m s
    -> JE.JSRep
    -> (DL.DList JE.Property)
    -> ReactMlT m a
    -> ReactMlT m a
bh' i s = branch (getListeners i s)


-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Core.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Core.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: MonadReactor m => Scene p m s -> m ()
dirty (Obj{..}) =
    doModifyIORef' self (my._plan._currentFrameNum %~ ((+ 1) . (`mod` JE.maxSafeInteger)))

rerender :: MonadReactor m => Scene p m s -> m ()
rerender (Obj{..}) = do
    me <- doReadIORef self
    let c = me ^. my._plan._currentFrameNum
        p = me ^. my._plan._previousFrameNum
    if c == p
        then pure ()
        else do
            doModifyIORef' self (my._plan._previousFrameNum .~ c)
            doSetComponentState
                (JE.fromProperties [("frameNum", JE.toJSR c)])
                (me ^. (my._plan._component))
