{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Activator where

import Control.Lens
-- import Data.Functor.Contravariant
import Data.Semigroup
import qualified Glazier.React.Framework.Core.IsReader as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F

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

type ObjActivator m v s = Activator m (F.Obj v s)
type SceneActivator x m v s = Activator m (F.Scene x m v s)

newtype ObjActivatorOnSpec m v s = ObjActivatorOnSpec { runObjActivatorOnSpec :: ObjActivator m v s}

type instance F.OnSpec (ObjActivatorOnSpec m v) s = ObjActivator m v s

instance F.ViaSpec (ObjActivatorOnSpec m v) where
    viaSpec l (Activator f) = Activator $ \obj ->
        f (F.edit l obj)
