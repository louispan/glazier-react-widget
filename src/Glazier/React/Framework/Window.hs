{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Window where

import Control.Lens
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F

newtype Window m s r = Window
    { runWindow :: s -> R.ReactMlT m r
    } deriving (Functor)

instance Monad m => Applicative (Window m s) where
    pure = Window . const . pure
    Window f <*> Window g = Window $ \s -> f s <*> g s

instance Monad m => Monad (Window m s) where
    Window f >>= k = Window $ \s -> f s >>= k' s
      where
        k' s = (`runWindow` s) . k

instance (Semigroup r, Monad m) => Semigroup (Window m s r) where
    Window a <> Window b = Window $ \s -> a s <> b s

instance (Monoid r, Monad m) => Monoid (Window m s r) where
    mempty =  Window . const $ pure mempty
    Window a `mappend` Window b = Window $ \s -> a s `mappend` b s

-----------------------------------------

newtype WindowOnModel m r s = WindowOnModel { runWindowOnModel :: Window m s r }

type instance F.OnModel (WindowOnModel m r) s = Window m s r

instance F.ViaModel (WindowOnModel m r) where
    viaModel l (Window f) = Window $ f . view l
