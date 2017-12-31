{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Finalizer where

import Control.Applicative
import Data.Functor.Contravariant
import Control.Disposable as CD
import Control.Lens
import Data.Semigroup
import qualified Glazier.React.Framework.Core as F

newtype Finalizer m s = Finalizer
    { runFinalizer :: s -> m CD.Disposable
    }

instance Contravariant (Finalizer m) where
    contramap f (Finalizer g) = Finalizer (g . f)

instance (Applicative m) => Semigroup (Finalizer m s) where
    Finalizer a <> Finalizer b = Finalizer $ liftA2 (liftA2 (<>)) a b

instance (Applicative m) => Monoid (Finalizer m s) where
    mempty =  Finalizer . pure $ pure mempty
    mappend = (<>)

-----------------------------------------

type instance F.OnModel (Finalizer m) s = Finalizer m s

instance F.ViaModel (Finalizer m) where
    viaModel l (Finalizer f) = Finalizer $ f . view l
