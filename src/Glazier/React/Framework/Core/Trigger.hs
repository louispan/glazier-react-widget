{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Trigger where

import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad.Trans
import qualified Data.DList as DL
import Data.Maybe
import Data.Semigroup
import qualified GHCJS.Types as J
import Glazier.Core
import Glazier.React
import Glazier.React.Framework.Core.Display
import Glazier.React.Framework.Core.Widget
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- -- | A simplified form of 'trigger' where all event info is dropped
-- -- and the given value is fired
-- trigger' :: (MonadReactor m)
--     => GadgetId
--     -> J.JSString
--     -> b
--     -> MethodT (Scene p m s) m b
-- trigger' gid n b = trigger gid n (const $ pure ()) (const b)

-- -- | Create callback for 'Notice' and add it to this state's dlist of listeners.
-- trigger ::
--     ( MonadReactor m
--     , NFData a
--     )
--     => GadgetId
--     -> J.JSString
--     -> (Notice -> IO a)
--     -> (a -> b)
--     -> MethodT (Scene p m s) m b
-- trigger gid n goStrict = mkListener gid n goStrict'
--   where
--     goStrict' e = case JE.fromJSR e of
--         Nothing -> pure Nothing
--         Just e' -> Just <$> goStrict e'


-- -- | Create callbacks and add it to this state's dlist of listeners.
-- -- NB. You probably want ot use 'trigger' instead since most React callbacks
-- -- generate a 'Notice'.
-- -- Only the "ref" callback generate 'EventTarget' in which case you would want
-- -- to use 'withRef' instead.
-- mkListener ::
--     ( NFData a
--     , MonadWidget x s m
--     )
--     => J.JSString
--     -> (JE.JSRep -> IO (Maybe a))
--     -> (a -> b)
--     -> m b
-- mkListener n goStrict goLazy = do
--     Traversal _myPln <- myPlan
--     methodT' $ \this@(Obj{..}) fire -> do
--     let goLazy' ma = case ma of
--             Nothing -> pure ()
--             Just a -> fire (goLazy a) ^*> rerender this
--     (ds, cb) <- doMkCallback goStrict goLazy'
--     doModifyIORef' self
--         $ my._plan._listeners.at gid %~ (\ls -> Just $ (n, cb) `DL.cons` (fromMaybe DL.empty ls))
--         >>> my._plan._disposeOnRemoved %~ (<> ds)

-- -- | This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an EventTarget
-- -- in the plan
-- withRef ::
--     ( MonadReactor m
--     )
--     => GadgetId
--     -> MethodT (Scene p m s) m ()
-- withRef i = mkListener i "ref" (pure . Just) id
--     >>= hdlRef
--   where
--     -- hdlRef :: SceneHandler p s m (EventTarget) (Which '[])
--     hdlRef j = readrT' $ \(Obj{..}) ->
--         lift $ doModifyIORef' self (my._plan._refs.at i .~ Just j)
