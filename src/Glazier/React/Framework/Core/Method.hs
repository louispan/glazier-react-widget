{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Glazier.React.Framework.Core.Method where

import Control.Lens
import Control.Monad.Trans.Delegate
import Control.Monad.Trans.Readr
import Control.Monad.Trans.State.Strict
import Glazier.React.Framework.Core.Model

type MethodT w x s m a = ReadrT (ReifiedTraversal' w (Scene x s)) (DelegateT (StateT w m)) a

-- pattern Method' :: ((a -> m ()) -> m ()) -> DelegateT m a
-- pattern Method' f = DelegateT (ContT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ReadrT_ #-}
-- #endif

-- readMy :: (Traversal' w (Scene x s) -> DelegateT (StateT w m) a) -> MethodT w x s m a
-- readMy f = do
--     Traversal my <- ask
--     lift $ f my

methodT' ::
    (Traversal' w (Scene x s)
    -> (a -> StateT w m ())
    -> StateT w m ())
    -> MethodT w x s m a
-- methodT' = readrT' . (delegateT' .) . (. runTraversal)
methodT' f = readrT' (\r -> delegateT' (f (runTraversal r)))

runMethodT' ::
    MethodT w x s m a
    -> Traversal' w (Scene x s)
    -> (a -> StateT w m ())
    -> StateT w m ()
-- runMethodT' = (runDelegateT' .) . runReadrT'
runMethodT' x l = runDelegateT' (runReadrT' x (Traversal l))
