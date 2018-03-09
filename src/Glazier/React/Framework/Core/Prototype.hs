{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Prototype where

import Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Semigroup
import Data.Semigroup.Applicative
import qualified GHC.Generics as G
import Glazier.Core
import Glazier.React.Framework.Core.Display
import Glazier.React.Framework.Core.Model

-- | Unforunately, because each widget contains callbacks
-- that has to be cleaned manually, we can't just rely on the garbage collector.
-- This contains 'Disposable' to be called on the *next* render.
-- This must be called before removing widgets from a containing model.
type Finalizer s m = s -> Ap m CD.Disposable

data Prototype p s m c = Prototype
    { display :: FrameDisplay s m ()
    , finalizer :: Finalizer s m
    , initializer :: MethodT (Scene p m s) m c
    } deriving (G.Generic, Functor)

makeLenses_ ''Prototype

withPrototype :: Monad m =>
    (MethodT (Scene p m s) m c1 -> MethodT (Scene p m s) m c2 -> MethodT (Scene p m s) m c3)
    -> Prototype p s m c1 -> Prototype p s m c2 -> Prototype p s m c3
withPrototype f (Prototype dis1 fin1 ini1) (Prototype dis2 fin2 ini2) =
    Prototype
    (dis1 <> dis2)
    (fin1 <> fin2)
    (f ini1 ini2)

------------------------------------------

instance Monad m => Applicative (Prototype p s m) where
    pure a = Prototype mempty (const $ pure mempty) (pure a)
    (<*>) = withPrototype (<*>)

instance Monad m => Semigroup (Prototype p s m c) where
    (<>) = withPrototype (<>)

instance Monad m => Monoid (Prototype p s m c) where
    mempty = Prototype mempty (const $ pure mempty) mempty
    mappend = withPrototype mappend

-- | Modify prototype's reading environment @s1@ inside a larger @s2@
magnifyPrototype :: Lens' s2 s1  -> Prototype p s1 m c -> Prototype p s2 m c
magnifyPrototype sl (Prototype disp fin ini) = Prototype
    -- (enlargeBuilder fi (view sl) bld)
    (magnify (editFrame sl) disp)
    (magnify sl fin)
    (magnify (to $ accessScene sl) ini)

-- | Makes and initialzies a spec from a req.
-- Used by prototypes that contain other archetypes.
mkInitializedSpec :: Monad m
    => (r -> m s)
    -> MethodT s m c
    -> MethodT r m (c, s)
mkInitializedSpec mkSpc ini = do
    r <- ask
    s <- lift $ lift $ mkSpc r
    c <- magnify (to (const s)) ini
    pure (c, s)


-- obviousHandler :: Handler s m a b -> Handler s m (Which '[a]) b
-- obviousHandler hdl = hdl . obvious

-- contramapHandler :: (a1 -> a2) -> Handler s m a2 b -> Handler s m a1 b
-- contramapHandler f hdl = hdl . f

-- mapHandler :: (b1 -> b2) -> Handler s m a b1 -> Handler s m a b2
-- mapHandler = fmap . fmap

-- memptyHandler :: Applicative m => Handler s m a b
-- memptyHandler = mempty

-- mappendHandler :: Applicative m => Handler s m a b -> Handler s m a b -> Handler s m a b
-- mappendHandler = mappend
-- infixr 6 `mappendHandler` -- like mappend

-- ignoreHandler :: forall a m s. Applicative m => Handler s m a ()
-- ignoreHandler = (const @_ @a) mempty

-- arrowHandler :: (a -> b) -> Handler s m a b
-- arrowHandler f = rk $ arr f

-- -- Chain the output from one handler into the input of the other.
--     Handler s m a b
--     -> Handler s m b c
--     -> Handler s m a c
-- -- intoH f g = f & E.rk2 (>>>) $ g
-- intoH f g = f >=> g

-- -- Chain the output from one handler into the input of the other
-- -- as much as possible. Any unhandled output is forwarded.
-- intoH' :: (Injected a2 b1 b2 b3)
--     => Handler s m a (Which b1)
--     -> Handler s m (Which a2) (Which b2)
--     -> Handler s m a (Which b3)
-- intoH' f g = f >=> E.underK1 injected g

-- -- Run th left handler and then the right handler with the same input,
-- -- and only fire events from the second input.
-- thenH :: Handler s m a () -> Handler s m a b -> Handler s m a b
-- thenH = ($!*>)

-- -- Run left and also the right handler with the same input, and combine the output type
-- -- A binary associative function for 'nulHandler'.
-- alsoH :: (Applicative m, ChooseBoth b1 b2 b3)
--     => Handler s m a (Which b1)
--     -> Handler s m a (Which b2)
--     -> Handler s m a (Which b3)
-- alsoH = liftA2 also
-- infixr 6 `alsoH` -- like mappend

-- maybeH :: Applicative m
--     => Handler s m a b
--     -> Handler s m (Maybe a) b
-- maybeH hdl = maybe mempty hdl
