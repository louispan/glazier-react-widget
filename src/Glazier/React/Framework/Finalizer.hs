{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Finalizer where

import Control.Applicative
import Control.Disposable as CD
import Control.Lens
import Data.Semigroup
import qualified Glazier.React.Framework.IsReader as F
import qualified Glazier.React.Framework.Model as F

-- type Finalizer m s = (s -> m CD.Disposable)

newtype Finalizer m s = Finalizer
    { runFinalizer :: s -> m CD.Disposable
    }

instance F.IsReader s (Finalizer m s) where
    type ReaderResult s (Finalizer m s) = m CD.Disposable
    fromReader = Finalizer
    toReader = runFinalizer

instance Contravariant (Finalizer m) where
    contramap f (Finalizer g) = Finalizer (g . f)

instance (Applicative m) => Semigroup (Finalizer m s) where
    Finalizer a <> Finalizer b = Finalizer $ liftA2 (liftA2 (<>)) a b

instance (Applicative m) => Monoid (Finalizer m s) where
    mempty =  Finalizer . pure $ pure mempty
    mappend = (<>)

-----------------------------------------

type instance F.OnSpec (Finalizer m) s = Finalizer m s

instance F.ViaSpec (Finalizer m) where
    viaSpec l (Finalizer f) = Finalizer $ f . view l
