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
import Control.Monad.Trans.Readers
import Control.Monad.Trans.States.Strict
import Glazier.React.Framework.Core.Model

-- | The type for initializing and handling callbacks.
-- @w@ world
-- @x@ commands to execute
-- @s@ actual model of widget
-- @m@ inner monad
-- @a@ return of monad
type GadgetT w x s m = ReadersT (ReifiedTraversal' w (Scene x s)) (DelegateT (StatesT w m))
type Gadget w x s = GadgetT w x s Identity

-- pattern Method' :: ((a -> m ()) -> m ()) -> DelegateT m a
-- pattern Method' f = DelegateT (ContT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ReadersT_ #-}
-- #endif

-- readMy :: (Traversal' w (Scene x s) -> DelegateT (StateT w m) a) -> MethodT w x s m a
-- readMy f = do
--     Traversal my <- ask
--     lift $ f my

gadgetT ::
    (Traversal' w (Scene x s)
    -> (a -> StatesT w m ())
    -> StatesT w m ())
    -> GadgetT w x s m a
-- methodT' = readrT' . (delegateT' .) . (. runTraversal)
gadgetT f = readersT (\r -> delegateT (f (runTraversal r)))

runGadgetT ::
    GadgetT w x s m a
    -> Traversal' w (Scene x s)
    -> (a -> StatesT w m ())
    -> StatesT w m ()
-- runMethodT' = (runDelegateT' .) . runReadersT'
runGadgetT x l = runDelegateT (runReadersT x (Traversal l))
