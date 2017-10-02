{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Glazier.React.Framework.Activator where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse
import Data.Kind
import Data.Proxy
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F

-- | If @commit@ is Nothing, then @cleanup@ must be run to cleanup handlers created.
type Activator s (i :: [Type]) ins (o :: [Type]) outs (c :: [Type]) cmds =
    F.Executor c cmds -- effectful interpreters
    -> F.Handler s o outs c cmds -- externally provided handlers
    -> TVar s -- TVar that contains the specs
    -> F R.Reactor (R.Disposable () {- cleanup -}, Maybe (STM ()) {- commit -})

-- | identity for 'andActivator'
inert :: Activator s '[] acts '[] cmds
inert _ _ _ = pure (pure (), Just (pure ()))

-- | It is okay to combine activators the expect the same @a@ action, hence the use of 'AppendUnique'
andActivator
    :: Activator s a1 acts c1 cmds
    -> Activator s a2 acts c2 cmds
    -> Activator s (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
andActivator f g exec hdl v = do
    f'@(_, m1) <- f (F.Executor (Proxy, F.getExecutor exec)) (F.Handler (Proxy, Proxy, F.getHandler hdl)) v
    case m1 of
        Nothing -> pure f'
        Just m1' -> do
            g' <- g (F.Executor (Proxy, F.getExecutor exec)) (F.Handler (Proxy, Proxy, F.getHandler hdl)) v
            pure $ liftA2 (>>) f' g'
