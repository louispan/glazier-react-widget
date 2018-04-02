{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Conts
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Typeable
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Framework.Gadget
import Glazier.React.Framework.MkId
import Glazier.React.Framework.Obj
import Glazier.React.Framework.Reactor
import Glazier.React.Framework.Scene
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | Create a callback and add it to this gizmo's dlist of listeners.
-- NB. You probably want to use 'trigger' instead since most React callbacks
-- generate a 'Notice'.
-- Only the "ref" callback generate 'EventTarget' or 'ComponentRef' in which case you would want
-- to use 'withRef' instead.
mkTick1 ::
    ( NFData a
    , AsFacet (MkTick1 c) c
    )
    => GizmoId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> States (Scenario c p) b)
    -> States (Scenario c p) ()
    -> Gadget c p s b
mkTick1 = mkTick1_ _listeners

mkOnceTick1 ::
    ( NFData a
    , AsFacet (MkTick1 c) c
    )
    => GizmoId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> States (Scenario c p) b)
    -> States (Scenario c p) ()
    -> Gadget c p s b
mkOnceTick1 = mkTick1_ _oncelisteners

mkTick1_ ::
    ( NFData a
    , AsFacet (MkTick1 c) c
    )
    => Lens' Gizmo (M.Map J.JSString (JE.JSRep -> IO ()))
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> States (Scenario c p) b)
    -> States (Scenario c p) ()
    -> Gadget c p s b
mkTick1_ l gid n goStrict goLazy extra = do
    SceneObj planObj modelObj <- ask
    lift $ contsT $ \fire -> do
        -- Add extra command producting state actions at the end
        let goLazy' a = (goLazy a >>= fire) *> extra
            cmd = MkTick1 (refer planObj) (refer modelObj) goStrict goLazy' $ \tick -> do
                let addListener = l.at n %~ (Just . (*> tick) . fromMaybe mempty)
                _plan.my planObj._gizmos.at gid %= (Just . addListener . fromMaybe newGizmo)
        post1' cmd

-- | A 'trigger' where all event info is dropped and the given value is fired.
-- trigger' ::
--     ( AsFacet (MkCallback1 c t) c
--     , AsFacet Rerender c
--     )
--     => GizmoId
--     -> J.JSString
--     -> b
--     -> Gadget c t s b
-- trigger' gid n b = do
--     Traversal my <- ask
--     -- Add a rerender for this widget at the every end
--     mkTick1' gid n (const $ pure (Just ())) (const $ pure b) (zoom my rerender)

trigger_ ::
    ( Typeable p
    , AsFacet Rerender c
    , AsFacet (MkTick1 c) c
    )
    => Lens' Gizmo (M.Map J.JSString (JE.JSRep -> IO ()))
    -> GizmoId
    -> J.JSString
    -> b
    -> Gadget c p s b
trigger_ l gid n b = do
    SceneObj planObj _ <- ask
    -- Add a rerender for this widget at the every end
    mkTick1_ l gid n (const $ pure (Just ())) (const $ pure b) (zoom (editPlan (my planObj)) rerender)

-- -- | Create callback for 'Notice' and add it to this gizmos's dlist of listeners.
-- -- Also adds a 'Rerender' command at the end of the callback
-- trigger ::
--     ( NFData a
--     , AsFacet (MkCallback1 c t) c
--     , AsFacet Rerender c
--     )
--     => GizmoId
--     -> J.JSString
--     -> (Notice -> IO a)
--     -> (a -> States (Scenario c t) b)
--     -> Gadget c t s b
-- trigger gid n goStrict goLazy = do
--     Traversal my <- ask
--     -- Add a rerender for this widget at the every end
--     mkListener gid n goStrict' goLazy (zoom my rerender)
--   where
--     goStrict' e = case JE.fromJSR e of
--         Nothing -> pure Nothing
--         Just e' -> Just <$> goStrict e'

-- -- | This adds a ReactJS "ref" callback assign the ref into an EventTarget for the
-- -- gizmo in the plan
-- withRef ::
--         ( AsFacet (MkCallback1 c t) c
--         )
--         => GizmoId
--         -> Gadget c t s ()
-- withRef gid = do
--     Traversal my <- ask
--     mkListener gid "ref" (pure . Just) (hdlRef my) (pure ())
--   where
--     hdlRef my j = let evt = JE.fromJSR j
--                in my._plan._gizmos.ix gid._targetRef .= evt

