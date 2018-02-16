{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Finalizer where

import Control.Applicative
import Control.Disposable as CD
import Data.Semigroup

type Finalizer m s = s -> m CD.Disposable

nulFinalizer :: Applicative m => Finalizer m s
nulFinalizer _ = pure mempty

andFinalizer :: Applicative m => Finalizer m s -> Finalizer m s -> Finalizer m s
andFinalizer f g s = liftA2 (<>) (f s) (g s)
infixr 6 `andFinalizer` -- like mappend
