{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Display where

import Control.Lens
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F

newtype Display m s r = Display
    { runDisplay :: s -> R.ReactMlT m r
    } deriving (Functor)

type ComDisplay x m s r = Display m (F.ComponentPlan x m, s) r

instance Monad m => Applicative (Display m s) where
    pure = Display . const . pure
    Display f <*> Display g = Display $ \s -> f s <*> g s

instance Monad m => Monad (Display m s) where
    Display f >>= k = Display $ \s -> f s >>= k' s
      where
        k' s = (`runDisplay` s) . k

instance (Semigroup r, Monad m) => Semigroup (Display m s r) where
    Display a <> Display b = Display $ \s -> a s <> b s

instance (Monoid r, Monad m) => Monoid (Display m s r) where
    mempty =  Display . const $ pure mempty
    Display a `mappend` Display b = Display $ \s -> a s `mappend` b s

-----------------------------------------

newtype DisplayOnModel m r s = DisplayOnModel { runDisplayOnModel :: Display m s r }

type instance F.OnModel (DisplayOnModel m r) s = Display m s r

instance F.ViaModel (DisplayOnModel m r) where
    viaModel l (Display f) = Display $ f . view l
