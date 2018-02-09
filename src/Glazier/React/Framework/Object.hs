{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Object where

import Control.Lens
import Data.IORef
import qualified Glazier.React.Framework.Core as F

-- | Uses ReifiedLens' to avoid impredicative polymorphism
data Object v s = Object {ref :: IORef v, this :: ReifiedLens' v s }

type ComObject x m v s = Object v (F.Plan x m, s)

nest :: Lens' s a -> Object v s -> Object v a
nest l obj = let Lens t = this obj in obj { this = Lens (t.l) }
