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
import qualified Glazier.React.Framework.IsReader as F
import qualified Glazier.React.Framework.Model as F

newtype Display m s r = Display
    { runDisplay :: s -> R.ReactMlT m r
    } deriving (Functor)

instance F.IsReader s (Display m s r) where
    type ReaderResult s (Display m s r) = R.ReactMlT m r
    fromReader = Display
    toReader = runDisplay

type FrameDisplay x m s r = Display m (F.Frame x m s) r

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

newtype DisplayOnSpec m r s = DisplayOnSpec { runDisplayOnSpec :: Display m s r }

type instance F.OnSpec (DisplayOnSpec m r) s = Display m s r

instance F.ViaSpec (DisplayOnSpec m r) where
    viaSpec l (Display f) = Display $ f . view l
