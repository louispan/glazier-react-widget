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
import qualified Glazier.Core as Z
import qualified Glazier.React as Z
import qualified Glazier.React.Framework.Core.Display as Z
import qualified Glazier.React.Framework.Core.Model as Z
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | A simplified form of 'trigger' where all event info is dropped
-- and the given value is fired
trigger' :: (Z.MonadReactor m)
    => Z.GadgetId
    -> J.JSString
    -> b
    -> Z.SceneInitializer m v s b
trigger' gid n b = trigger gid n (const $ pure ()) (const b)

-- | Create callback for 'Z.SyntheticEvent' and add it to this state's dlist of listeners.
trigger ::
    ( Z.MonadReactor m
    , NFData a
    )
    => Z.GadgetId
    -> J.JSString
    -> (Z.SyntheticEvent -> IO a)
    -> (a -> b)
    -> Z.SceneInitializer m v s b
trigger gid n goStrict goLazy = mkListener gid n goStrict' goLazy
  where
    goStrict' e = case JE.fromJSR e of
        Nothing -> pure Nothing
        Just e' -> Just <$> goStrict e'

-- | Create callbacks and add it to this state's dlist of listeners.
-- NB. You probably want ot use 'trigger' instead since most React callbacks
-- generate a 'Z.SyntheticEvent'.
-- Only the "ref" callback generate 'Z.EventTarget' in which case you would want
-- to use 'withRef' instead.
mkListener ::
    ( Z.MonadReactor m
    , NFData a
    )
    => Z.GadgetId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> b)
    -> Z.SceneInitializer m v s b
mkListener gid n goStrict goLazy this@(Z.Obj ref its) = ContT $ \fire -> do
    (ds, cb) <- Z.doMkCallback goStrict (goLazy' fire)
    Z.doModifyIORef' ref $ \obj ->
        obj & its.Z.plan.field @"listeners".at gid %~ (\ls -> Just $ (n, cb) `DL.cons` (fromMaybe DL.empty ls))
            & its.Z.plan.field @"disposeOnRemoved" %~ (<> ds)
  where
    goLazy' fire ma = case ma of
        Nothing -> pure ()
        Just a -> fire (goLazy a) *> Z.rerender this

-- | feed the result from an Initializer into a handler, from left to right.
-- A simply way to think of the types is:
-- @
-- handledBy :: Z.Initializer m s a -> Z.Handler m s a b -> Z.Initializer m s b
-- @
handledBy :: (Applicative f, Monad m) => f (m a) -> f (a -> m b) -> f (m b)
handledBy ini hdl = liftA2 (>>=) ini hdl
infixl 1 `handledBy` -- like >>=

-- | feed the result from an Initializer into a handler, from right to left.
handles :: Z.Handler m s a b -> Z.Initializer m s a -> Z.Initializer m s b
handles = flip handledBy
infixr 1 `handles` -- like =<<

-- | feed as much of the result from an Initializer into a handler,
-- from left to right.
handledBy' :: forall m s a1 a2 b2 b3.
    (Z.Pretend a2 a1 b2 b3)
    => Z.Initializer m s (Which a1)
    -> Z.Handler m s (Which a2) (Which b2)
    -> Z.Initializer m s (Which b3)
handledBy' ini hdl = ini `handledBy` (Z.pretend @a1 hdl)
infixl 1 `handledBy'` -- like >>=

-- | feed as much of the result from an Initializer into a handler,
-- from right to left.
handles' ::
    (Z.Pretend a2 a1 b2 b3)
    => Z.Handler m s (Which a2) (Which b2)
    -> Z.Initializer m s (Which a1)
    -> Z.Initializer m s (Which b3)
handles' = flip handledBy'
infixr 1 `handles'` -- like =<<

-- | This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an Z.EventTarget
-- in the plan
withRef ::
    ( Z.MonadReactor m
    )
    => Z.GadgetId
    -> Z.SceneInitializer m v s (Which '[])
withRef i = mkListener i "ref" (pure . Just . Z.EventTarget) id
    `handledBy` hdlRef
  where
    -- hdlRef :: Z.SceneHandler m v s (Z.EventTarget) (Which '[])
    hdlRef (Z.Obj ref its) j = terminate' . lift $
        Z.doModifyIORef' ref (its.Z.plan.field @"refs".at i .~ Just j)

-- | Convert the original ContT to a ContT that
-- doens't call it's continuation, by 'const'ing the original contination
-- to 'pure'.
-- This is useful for converting a @Handler m s a ()@ to a @Handler m s a (Which '[])@
-- for combining using 'orHandler'.
-- This ContT can be run with  'Data.Diverse.Which.impossible'.
-- @ContT r m (Which '[])@ is effectively equivanlent to @m r@
terminate' :: Applicative m => ContT () m () -> ContT () m (Which '[])
terminate' = TE.terminate @(Which '[])
