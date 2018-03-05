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
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React.Framework.Core.Display as Z
import qualified Glazier.React.Framework.Core.Finalizer as Z
import qualified Glazier.React.Framework.Core.Initializer as Z
import qualified Glazier.React.Framework.Core.Model as Z

data Prototype v s m c = Prototype
    { display :: Z.FrameDisplay s m ()
    , finalizer :: Z.Finalizer s m
    , initializer :: Z.SceneInitializer v s m c
    } deriving (G.Generic, Functor)

modifyPDisplay :: (Z.FrameDisplay s m () -> Z.FrameDisplay s m ())
    -> Prototype v s m c -> Prototype v s m c
modifyPDisplay f p = let disp = display p in p { display = f disp }
infixl 4 `modifyPDisplay` -- like <$>

modifyPFinalizer :: (Z.Finalizer s m -> Z.Finalizer s m)
    -> Prototype v s m c -> Prototype v s m c
modifyPFinalizer f p = let fin = finalizer p in p { finalizer = f fin }
infixl 4 `modifyPFinalizer` -- like <$>

modifyPInitializer :: (Z.SceneInitializer v s m c1 -> Z.SceneInitializer v s m c2)
    -> Prototype v s m c1 -> Prototype v s m c2
modifyPInitializer f p = let ini = initializer p in p { initializer = f ini }
infixl 4 `modifyPInitializer` -- like <$>

modifyPInitializer2 :: Monad m =>
    (Z.SceneInitializer v s m c1 -> Z.SceneInitializer v s m c2 -> Z.SceneInitializer v s m c3)
    -> Prototype v s m c1 -> Prototype v s m c2 -> Prototype v s m c3
modifyPInitializer2 f (Prototype dis1 fin1 ini1) (Prototype dis2 fin2 ini2) =
    Prototype
    (dis1 <> dis2)
    (fin1 <> fin2)
    (f ini1 ini2)

------------------------------------------

instance Monad m => Applicative (Prototype v s m) where
    pure a = Prototype mempty mempty (pure a)
    (<*>) = modifyPInitializer2 (<*>)

instance Monad m => Semigroup (Prototype s v m c) where
    (<>) = modifyPInitializer2 (<>)

instance Monad m => Monoid (Prototype s v m c) where
    mempty = Prototype mempty mempty mempty
    mappend = modifyPInitializer2 mappend

-- | Modify prototype's reading environment @s1@ inside a larger @s2@
magnifyPrototype :: Monad m => Lens' s2 s1  -> Prototype v s1 m c -> Prototype v s2 m c
magnifyPrototype sl (Prototype disp fin ini) = Prototype
    -- (Z.enlargeBuilder fi (view sl) bld)
    (magnify (alongside id sl) disp)
    (magnify sl fin)
    (magnify (to (Z.editScene sl)) ini)

-- | Makes and initialzies a spec from a req.
-- Used by prototypes that contain other archetypes.
mkInitializedSpec :: Monad m
    => ReaderT r m s
    -> Z.Initializer s m c
    -> Z.Initializer r m (c, s)
mkInitializedSpec mkSpc ini = do
    r <- ask
    s <- lift $ runReaderT mkSpc r
    c <- magnify (to (const s)) ini
    pure (c, s)
