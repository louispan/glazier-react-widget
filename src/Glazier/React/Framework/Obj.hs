{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Obj where

import Control.Lens
import Data.IORef

-- | Uses ReifiedLens' to avoid impredicative polymorphism
data Obj v s = Obj {ref :: IORef v, its :: ReifiedLens' v s }

edit :: Lens' s a -> Obj v s -> Obj v a
edit l obj = let Lens t = its obj in obj { its = Lens (t.l) }
