-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Trigger where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Data.Diverse.Profunctor
import Data.Generics.Product
import Data.Maybe
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Event.Internal as R
import qualified Glazier.React.Framework.Core.Activator as F
import qualified Glazier.React.Framework.Core.Handler as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | Create callbacks and add it to this state's dlist of listeners.
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
trigger :: forall m v s a.
    ( R.MonadReactor m
    , NFData a
    )
    => F.WidgetId
    -> J.JSString
    -> (J.JSVal -> IO a)
    -> F.SceneActivator m v s a
trigger i n f = \(F.Obj ref its) -> ContT $ \fire -> do
    (ds, cb) <- R.doMkCallback f fire
    R.doModifyIORef' ref $ \obj ->
        obj & its.F.plan.field @"listeners".at i %~ (\ls -> Just $ (n, cb) : (fromMaybe [] ls))
            & its.F.plan.field @"disposeOnRemoved" %~ (<> ds)

drives :: F.Activator m s a -> F.Handler m s a b -> F.Activator m s b
drives act hdl = liftA2 (>>=) act hdl
infixl 1 `drives` -- like >>=

controls :: F.Handler m s a b -> F.Activator m s a -> F.Activator m s b
controls = flip drives
infixr 1 `controls` -- like =<<

drives' :: forall m s a1 a2 b2 b3.
    (F.Pretend a2 a1 b2 b3)
    => F.Activator m s (Which a1)
    -> F.Handler m s (Which a2) (Which b2)
    -> F.Activator m s (Which b3)
drives' act hdl = act `drives` (F.pretend @a1 hdl)
infixl 1 `drives'` -- like >>=

controls' :: forall m s a1 a2 b2 b3.
    (F.Pretend a2 a1 b2 b3)
    => F.Handler m s (Which a2) (Which b2)
    -> F.Activator m s (Which a1)
    -> F.Activator m s (Which b3)
controls' = flip drives'
infixr 1 `controls'` -- like =<<

-- | Convenience function to create an activator
-- given triggers and a handler.
controlledTrigger :: forall m v s a b.
    ( R.MonadReactor m
    , NFData a
    )
    => F.WidgetId
    -> J.JSString
    -> (J.JSVal -> IO a)
    -> F.SceneHandler m v s a b
    -> F.SceneActivator m v s b
controlledTrigger i n f hdl = (trigger i n f) `drives` hdl

-- | This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an R.EventTarget
-- in the plan
withRef ::
    ( R.MonadReactor m
    )
    => F.WidgetId
    -> F.SceneActivator m v s (Which '[])
withRef i = controlledTrigger i "ref"
    (pure. R.EventTarget . JE.JSVar) -- requires Internal
    hdlRef
  where
    -- hdlRef :: F.SceneHandler m v s (R.EventTarget) (Which '[])
    hdlRef (F.Obj ref its) j = F.terminate' . lift $
        R.doModifyIORef' ref (its.F.plan.field @"refs".at i .~ Just j)

-- -- | Convenience function to create an activator
-- -- given triggers and a handler.
-- -- Complex version using 'controls'
-- controlledTrigger :: forall t m v s c1 c2 c3 c4 a b.
--     ( R.MonadReactor m
--     , NFData (Which c2)
--     , HasItemTag' t [R.Listener] s
--     , c4 ~ AppendUnique c1 c3
--     , Injected a c2 b c3
--     , Diversify c1 c4
--     , Diversify c3 c4
--     )
--     => J.JSString
--     -> (J.JSVal -> IO (DL.DList (Which c2)))
--     -> ProtoHandler m v s (Which c1) (Which a) (Which b)
--     -> ProtoActivator m v s (Which c4)
-- controlledTrigger n f hdl = hdl `controls` trigger @t n f

