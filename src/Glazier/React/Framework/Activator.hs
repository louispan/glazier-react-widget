{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Activator where

import Control.Lens
-- import Data.Functor.Contravariant
import Data.Semigroup
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.IsReader as F
import qualified Glazier.React.Framework.Object as F

------------------------------------------

newtype Activator m r = Activator
    { runActivator :: r -- Handler env
                   -> m () -- return the monadic action to commit the activation
    }

instance F.IsReader r (Activator m r) where
    type ReaderResult r (Activator m r) = m ()
    fromReader = Activator
    toReader = runActivator

instance Applicative m => Semigroup (Activator m r) where
    (Activator f) <> (Activator g) = Activator $ \env ->
        f env *>
        g env

instance Applicative m => Monoid (Activator m r) where
    mempty = Activator $ \_ -> pure ()
    mappend = (<>)

instance Contravariant (Activator m) where
    contramap f (Activator act) = Activator $ act . f

------------------------------------------

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type ObjActivator m v s = Activator m (F.Object v s)
type ComActivator x m v s = Activator m (F.ComObject x m v s)

newtype ObjActivatorOnModel m v s = ObjActivatorOnModel { runObjActivatorOnModel :: ObjActivator m v s}

type instance F.OnModel (ObjActivatorOnModel m v) s = ObjActivator m v s

instance F.ViaModel (ObjActivatorOnModel m v) where
    viaModel l (Activator f) = Activator $ \obj ->
        f (F.nest l obj)
