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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Prototype where

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Generics.Product
import Data.Semigroup
import qualified GHC.Generics as G
import Glazier.Core
import Glazier.React.Framework.Core.Display
import Glazier.React.Framework.Core.Finalizer
import Glazier.React.Framework.Core.Model

data Prototype p s m c = Prototype
    { display :: FrameDisplay s m ()
    , finalizer :: Finalizer s m
    , initializer :: Delegate (Scene p m s) m c
    } deriving (G.Generic, Functor)

_display :: Lens' (Prototype p s m c) (FrameDisplay s m ())
_display = field @"display"

_finalizer :: Lens' (Prototype p s m c) (Finalizer s m)
_finalizer = field @"finalizer"

_initializer :: Lens (Prototype p s m c) (Prototype p s m c')
    (Delegate (Scene p m s) m c) (Delegate (Scene p m s) m c')
_initializer = field @"initializer"

-- modifyDisplay :: (FrameDisplay s m () -> FrameDisplay s m ())
--     -> Prototype p s m c -> Prototype p s m c
-- modifyDisplay f p = let disp = display p in p { display = f disp }
-- infixl 4 `modifyDisplay` -- like <$>

-- modifyFinalizer :: (Finalizer s m -> Finalizer s m)
--     -> Prototype p s m c -> Prototype p s m c
-- modifyFinalizer f p = let fin = finalizer p in p { finalizer = f fin }
-- infixl 4 `modifyFinalizer` -- like <$>

-- modifyInitializer :: (Delegate (Scene p m s) m c1 -> Delegate (Scene p m s) m c2)
--     -> Prototype p s m c1 -> Prototype p s m c2
-- modifyInitializer f p = let ini = initializer p in p { initializer = f ini }
-- infixl 4 `modifyInitializer` -- like <$>

withPrototype :: Monad m =>
    (Delegate (Scene p m s) m c1 -> Delegate (Scene p m s) m c2 -> Delegate (Scene p m s) m c3)
    -> Prototype p s m c1 -> Prototype p s m c2 -> Prototype p s m c3
withPrototype f (Prototype dis1 fin1 ini1) (Prototype dis2 fin2 ini2) =
    Prototype
    (dis1 <> dis2)
    (fin1 <> fin2)
    (f ini1 ini2)

------------------------------------------

instance Monad m => Applicative (Prototype p s m) where
    pure a = Prototype mempty mempty (pure a)
    (<*>) = withPrototype (<*>)

instance Monad m => Semigroup (Prototype p s m c) where
    (<>) = withPrototype (<>)

instance Monad m => Monoid (Prototype p s m c) where
    mempty = Prototype mempty mempty mempty
    mappend = withPrototype mappend

-- | Modify prototype's reading environment @s1@ inside a larger @s2@
magnifyPrototype :: Monad m => Lens' s2 s1  -> Prototype p s1 m c -> Prototype p s2 m c
magnifyPrototype sl (Prototype disp fin ini) = Prototype
    -- (enlargeBuilder fi (view sl) bld)
    (magnify (editFrame sl) disp)
    (magnify sl fin)
    (magnify (to $ accessScene sl) ini)

-- | Makes and initialzies a spec from a req.
-- Used by prototypes that contain other archetypes.
mkInitializedSpec :: Monad m
    => ReaderT r m s
    -> Delegate s m c
    -> Delegate r m (c, s)
mkInitializedSpec mkSpc ini = do
    r <- ask
    s <- lift $ runReaderT mkSpc r
    c <- magnify (to (const s)) ini
    pure (c, s)
