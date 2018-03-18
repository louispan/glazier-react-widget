{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Glazier.React.Framework.Core.Gadget where

import Control.Lens
import Control.Monad.Trans.Delegate
import Control.Monad.Trans.Readr
import Control.Monad.Trans.State.Strict
import Glazier.React.Framework.Core.Model

-- | The type for initializing and handling callbacks.
-- @w@ world
-- @x@ commands to execute
-- @s@ actual model of widget
-- @m@ inner monad
-- @a@ return of monad
type GadgetT w x s m = ReadrT (ReifiedTraversal' w (Scene x s)) (DelegateT (StateT w m))

-- pattern Method' :: ((a -> m ()) -> m ()) -> DelegateT m a
-- pattern Method' f = DelegateT (ContT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ReadrT_ #-}
-- #endif

-- readMy :: (Traversal' w (Scene x s) -> DelegateT (StateT w m) a) -> MethodT w x s m a
-- readMy f = do
--     Traversal my <- ask
--     lift $ f my

gadgetT ::
    (Traversal' w (Scene x s)
    -> (a -> StateT w m ())
    -> StateT w m ())
    -> GadgetT w x s m a
-- methodT' = readrT' . (delegateT' .) . (. runTraversal)
gadgetT f = readrT' (\r -> delegateT' (f (runTraversal r)))

runGadgetT ::
    GadgetT w x s m a
    -> Traversal' w (Scene x s)
    -> (a -> StateT w m ())
    -> StateT w m ()
-- runMethodT' = (runDelegateT' .) . runReadrT'
runGadgetT x l = runDelegateT' (runReadrT' x (Traversal l))
