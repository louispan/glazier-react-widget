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
trigger' :: forall m v s a.
    ( R.MonadReactor m
    , NFData a
    )
    => F.GadgetId
    -> J.JSString
    -> (J.JSVal -> IO a)
    -> F.SceneActivator m v s a
trigger' i n f = trigger i n f id

-- | Create callbacks and add it to this state's dlist of listeners.
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
trigger :: forall m v s a b.
    ( R.MonadReactor m
    , NFData a
    )
    => F.GadgetId
    -> J.JSString
    -> (J.JSVal -> IO a)
    -> (a -> b)
    -> F.SceneActivator m v s b
trigger i n f g = \(F.Obj ref its) -> ContT $ \fire -> do
    (ds, cb) <- R.doMkCallback f (fire . g)
    R.doModifyIORef' ref $ \obj ->
        obj & its.F.plan.field @"listeners".at i %~ (\ls -> Just $ (n, cb) : (fromMaybe [] ls))
            & its.F.plan.field @"disposeOnRemoved" %~ (<> ds)

-- | feed the result from an activator into a handler, from left to right.
-- A simply way to think of the types is:
-- @
-- activates :: F.Activator m s a -> F.Handler m s a b -> F.Activator m s b
-- @
activates :: (Applicative f, Monad m) => f (m a) -> f (a -> m b) -> f (m b)
activates act hdl = liftA2 (>>=) act hdl
infixl 1 `activates` -- like >>=

-- | feed the result from an activator into a handler, from right to left.
handles :: F.Handler m s a b -> F.Activator m s a -> F.Activator m s b
handles = flip activates
infixr 1 `handles` -- like =<<

-- | feed as much of the result from an activator into a handler,
-- from left to right.
activates' :: forall m s a1 a2 b2 b3.
    (F.Pretend a2 a1 b2 b3)
    => F.Activator m s (Which a1)
    -> F.Handler m s (Which a2) (Which b2)
    -> F.Activator m s (Which b3)
activates' act hdl = act `activates` (F.pretend @a1 hdl)
infixl 1 `activates'` -- like >>=

-- | feed as much of the result from an activator into a handler,
-- from right to left.
handles' ::
    (F.Pretend a2 a1 b2 b3)
    => F.Handler m s (Which a2) (Which b2)
    -> F.Activator m s (Which a1)
    -> F.Activator m s (Which b3)
handles' = flip activates'
infixr 1 `handles'` -- like =<<

-- | This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an R.EventTarget
-- in the plan
withRef ::
    ( R.MonadReactor m
    )
    => F.GadgetId
    -> F.SceneActivator m v s (Which '[])
withRef i = trigger' i "ref" (pure. R.EventTarget . JE.JSVar) -- requires Internal
    `activates` hdlRef
  where
    -- hdlRef :: F.SceneHandler m v s (R.EventTarget) (Which '[])
    hdlRef (F.Obj ref its) j = terminate' . lift $
        R.doModifyIORef' ref (its.F.plan.field @"refs".at i .~ Just j)

-- | Convert the original ContT to a ContT that
-- doens't call it's continuation, by 'const'ing the original contination
-- to 'pure'.
terminate :: forall b m . Applicative m => ContT () m () -> ContT () m b
terminate = withContT (const $ pure)

-- A variation of 'terminate' which  also fixes the result a @(Which '[])@.
-- This is useful for converting a @Handler m s a ()@ to a @Handler m s a (Which '[])@
-- for combining using 'orHandler'.
-- This ContT can be run with  'Data.Diverse.Which.impossible'.
-- @ContT r m (Which '[])@ is effectively equivanlent to @m r@
terminate' :: Applicative m => ContT () m () -> ContT () m (Which '[])
terminate' = terminate @(Which '[])

