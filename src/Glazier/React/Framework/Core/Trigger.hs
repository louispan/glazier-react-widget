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
import qualified Control.Monad.Trans.Cont.Extras as TE
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Generics.Product
import Data.Maybe
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Display as R
import qualified Glazier.React.Framework.Core.Handler as R
import qualified Glazier.React.Framework.Core.Initializer as R
import qualified Glazier.React.Framework.Core.Model as R
import qualified Glazier.React.Framework.Core.Obj as R
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | Create callbacks and add it to this state's dlist of listeners.
trigger' :: forall m v s a.
    ( R.MonadReactor m
    , NFData a
    )
    => R.GadgetId
    -> J.JSString
    -> (JE.JSRep -> IO a)
    -> R.SceneInitializer m v s a
trigger' i n f = trigger i n f id

-- | Create callbacks and add it to this state's dlist of listeners.
trigger :: forall m v s a b.
    ( R.MonadReactor m
    , NFData a
    )
    => R.GadgetId
    -> J.JSString
    -> (JE.JSRep -> IO a)
    -> (a -> b)
    -> R.SceneInitializer m v s b
trigger i n f g = \this@(R.Obj ref its) -> ContT $ \fire -> do
    (ds, cb) <- R.doMkCallback f (\a -> (fire (g a)) *> R.rerender this)
    R.doModifyIORef' ref $ \obj ->
        obj & its.R.plan.field @"listeners".at i %~ (\ls -> Just $ (n, cb) `DL.cons` (fromMaybe DL.empty ls))
            & its.R.plan.field @"disposeOnRemoved" %~ (<> ds)

-- | feed the result from an Initializer into a handler, from left to right.
-- A simply way to think of the types is:
-- @
-- handledBy :: R.Initializer m s a -> R.Handler m s a b -> R.Initializer m s b
-- @
handledBy :: (Applicative f, Monad m) => f (m a) -> f (a -> m b) -> f (m b)
handledBy ini hdl = liftA2 (>>=) ini hdl
infixl 1 `handledBy` -- like >>=

-- | feed the result from an Initializer into a handler, from right to left.
handles :: R.Handler m s a b -> R.Initializer m s a -> R.Initializer m s b
handles = flip handledBy
infixr 1 `handles` -- like =<<

-- | feed as much of the result from an Initializer into a handler,
-- from left to right.
handledBy' :: forall m s a1 a2 b2 b3.
    (R.Pretend a2 a1 b2 b3)
    => R.Initializer m s (Which a1)
    -> R.Handler m s (Which a2) (Which b2)
    -> R.Initializer m s (Which b3)
handledBy' ini hdl = ini `handledBy` (R.pretend @a1 hdl)
infixl 1 `handledBy'` -- like >>=

-- | feed as much of the result from an Initializer into a handler,
-- from right to left.
handles' ::
    (R.Pretend a2 a1 b2 b3)
    => R.Handler m s (Which a2) (Which b2)
    -> R.Initializer m s (Which a1)
    -> R.Initializer m s (Which b3)
handles' = flip handledBy'
infixr 1 `handles'` -- like =<<

-- | This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an R.EventTarget
-- in the plan
withRef ::
    ( R.MonadReactor m
    )
    => R.GadgetId
    -> R.SceneInitializer m v s (Which '[])
withRef i = trigger' i "ref" (pure . R.EventTarget) -- requires Internal
    `handledBy` hdlRef
  where
    -- hdlRef :: R.SceneHandler m v s (R.EventTarget) (Which '[])
    hdlRef (R.Obj ref its) j = terminate' . lift $
        R.doModifyIORef' ref (its.R.plan.field @"refs".at i .~ Just j)


-- | Convert the original ContT to a ContT that
-- doens't call it's continuation, by 'const'ing the original contination
-- to 'pure'.
-- This is useful for converting a @Handler m s a ()@ to a @Handler m s a (Which '[])@
-- for combining using 'orHandler'.
-- This ContT can be run with  'Data.Diverse.Which.impossible'.
-- @ContT r m (Which '[])@ is effectively equivanlent to @m r@
terminate' :: Applicative m => ContT () m () -> ContT () m (Which '[])
terminate' = TE.terminate @(Which '[])
