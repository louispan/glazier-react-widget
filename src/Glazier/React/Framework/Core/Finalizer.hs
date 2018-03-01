{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Finalizer where

import Control.Applicative
import Control.Disposable as CD
import Data.Semigroup

-- | Unforunately, because each widget contains callbacks
-- that has to be cleaned manually, we can't just rely on the garbage collector.
-- This contains 'Disposable' to be called on the *next* render.
-- This must be called before removing widgets from a containing model.
type Finalizer m s = s -> m CD.Disposable

nulFinalizer :: Applicative m => Finalizer m s
nulFinalizer _ = pure mempty

andFinalizer :: Applicative m => Finalizer m s -> Finalizer m s -> Finalizer m s
andFinalizer f g s = liftA2 (<>) (f s) (g s)
infixr 6 `andFinalizer` -- like mappend
