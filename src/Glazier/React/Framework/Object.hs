{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Object where

import Control.Lens
import Data.IORef

-- | Uses ReifiedLens' to avoid impredicative polymorphism
data Object v s = Object {ref :: IORef v, this :: ReifiedLens' v s }

withMember :: Lens' s a -> Object v s -> Object v a
withMember l obj = let Lens t = this obj in obj { this = Lens (t.l) }