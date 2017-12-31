{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Activator where

import Control.Lens
import Data.IORef
import Data.Semigroup
import qualified Glazier.React.Framework.Core as F

------------------------------------------

newtype Activator m r = Activator
    { runActivator :: r -- Handler env
                   -> m () -- return the monadic action to commit the activation
    }

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type RefActivator m v s = Activator m (IORef v, ReifiedLens' v s)

instance Monad m => Semigroup (Activator m r) where
    (Activator f) <> (Activator g) = Activator $ \env ->
        f env >>
        g env

instance Monad m => Monoid (Activator m r) where
    mempty = Activator $ \_ -> pure ()
    mappend = (<>)

instance Contravariant (Activator m) where
    contramap f (Activator act) = Activator $ act . f

newtype ActivatorOnModel m v s = ActivatorOnModel { runActivatorOnModel :: RefActivator m v s}

type instance F.OnModel (ActivatorOnModel m v) s = RefActivator m v s

instance F.ViaModel (ActivatorOnModel m v) where
    viaModel l (Activator f) = Activator $ \(ref, Lens this) ->
        f (ref, Lens (this.l))
