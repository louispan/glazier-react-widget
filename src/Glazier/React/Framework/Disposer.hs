{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Disposer where

import Control.Applicative
import Data.Functor.Contravariant
import Control.Disposable as CD
import Control.Lens
import Data.Semigroup
import qualified Glazier.React.Framework.Core as F

newtype Disposer m s = Disposer
    { runDisposer :: s -> m CD.Disposable
    }

instance Contravariant (Disposer m) where
    contramap f (Disposer g) = Disposer (g . f)

instance (Applicative m) => Semigroup (Disposer m s) where
    Disposer a <> Disposer b = Disposer $ liftA2 (liftA2 (<>)) a b

instance (Applicative m) => Monoid (Disposer m s) where
    mempty =  Disposer . pure $ pure mempty
    mappend = (<>)

-----------------------------------------

type instance F.OnModel (Disposer m) s = Disposer m s

instance F.ViaModel (Disposer m) where
    viaModel l (Disposer f) = Disposer $ f . view l
