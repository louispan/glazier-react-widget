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

module Glazier.React.Framework.Trigger
    ( trigger
    , triggerOnce
    , trigger'
    , triggerOnce'
    , withRef
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Conts
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import Data.Maybe
import Data.Tagged
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
mkAction1 ::
    ( NFData a
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Every" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> States (Scenario c p) b)
    -> States (Scenario c p) ()
    -> Gadget c p s b
mkAction1 l gid n goStrict goLazy extra = do
    SceneObj planVar mdl <- ask
    lift $ contsT $ \fire -> do
        -- Add extra command producting state actions at the end
        let goLazy' a = command' $ TickState planVar (ref mdl) ((goLazy a >>= fire) *> extra)
            cmd = MkAction1 goStrict goLazy' $ \act ->
                let addListener = _listeners2.at n %~ (Just . addAction . fromMaybe (Tagged mempty, Tagged mempty))
                    addAction acts = acts & l %~ (*> act)
                in command' $ TickState planVar (ref mdl) (_scene._plan._gizmos.at gid %= (Just . addListener . fromMaybe newGizmo))
        post1 cmd

-- | A 'trigger' where all event info is dropped and the given value is fired.
trigger' ::
    ( Typeable p
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => GizmoId
    -> J.JSString
    -> b
    -> Gadget c p s b
trigger' = trigger'_ (_2._Wrapped' @(Tagged "Every" _))

triggerOnce' ::
    ( Typeable p
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => GizmoId
    -> J.JSString
    -> b
    -> Gadget c p s b
triggerOnce' = trigger'_ (_1._Wrapped' @(Tagged "Once" _))

trigger'_ ::
    ( Typeable p
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Every" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> b
    -> Gadget c p s b
trigger'_ l gid n b =
    -- Add a rerender for this widget at the every end
    mkAction1 l gid n (const $ pure (Just ())) (const $ pure b) rerender

-- | Create callback for 'Notice' and add it to this gizmos's dlist of listeners.
-- Also adds a 'Rerender' command at the end of the callback
trigger ::
    ( NFData a
    , Typeable p
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> States (Scenario c p) b)
    -> Gadget c p s b
trigger = trigger_ (_2._Wrapped' @(Tagged "Every" _))

triggerOnce ::
    ( NFData a
    , Typeable p
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> States (Scenario c p) b)
    -> Gadget c p s b
triggerOnce = trigger_ (_1._Wrapped' @(Tagged "Once" _))

trigger_ ::
    ( NFData a
    , Typeable p
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => Lens' (Tagged "Once" (JE.JSRep -> IO ()), Tagged "Every" (JE.JSRep -> IO ())) (JE.JSRep -> IO ())
    -> GizmoId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> States (Scenario c p) b)
    -> Gadget c p s b
trigger_ l gid n goStrict goLazy =
    -- Add a rerender for this widget at the every end
    mkAction1 l gid n goStrict' goLazy rerender
  where
    goStrict' e = case JE.fromJSR e of
        Nothing -> pure Nothing
        Just e' -> Just <$> goStrict e'

-- | This adds a ReactJS "ref" callback assign the ref into an EventTarget for the
-- gizmo in the plan
withRef ::
    ( AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    )
    => GizmoId
    -> Gadget c p s ()
withRef gid =
    mkAction1 (_2._Wrapped' @(Tagged "Every" _)) gid "ref" (pure . Just) hdlRef (pure ())
  where
    hdlRef j = let evt = JE.fromJSR j
            in _scene._plan._gizmos.ix gid._targetRef .= evt

