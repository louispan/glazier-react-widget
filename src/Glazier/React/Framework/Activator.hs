{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Glazier.React.Framework.Activator where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Diverse
import Data.Kind
import Data.IORef
import Data.Proxy
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F

-- | State contains the cleanup code if activation failed
-- otherwise the return is the monad to run to commit the activation.
type Activator' m s a c
    = F.Executor' m c -- effectful interpreters
    -> F.Handler' m s a c -- externally provided handlers
    -> IORef s -- IORef that contains the specs
    -> MaybeT (StateT (R.Disposable ()) m) ()

-- | If @commit@ is Nothing, then @cleanup@ must be run to cleanup handlers created.
newtype Activator m s (a :: [Type]) acts (c :: [Type]) cmds = Activator
    ( Proxy a
    , Proxy c
    , Activator' m s acts cmds)

runActivator :: Activator m s a acts c cmds -> Activator' m s acts cmds
runActivator (Activator (_, _, f)) = f

-- | identity for 'andActivator'
inert :: Monad m => Activator m s '[] acts '[] cmds
inert = Activator (Proxy, Proxy, \_ _ _ -> pure ())

-- | It is okay to combine activators the expect the same @a@ action, hence the use of 'AppendUnique'
andActivator
    :: Monad m
    => Activator m s a1 acts c1 cmds
    -> Activator m s a2 acts c2 cmds
    -> Activator m s (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
andActivator (Activator (_, _, f)) (Activator (_, _, g)) = Activator (Proxy, Proxy, go)
  where
    go exec hdl v = MaybeT $ do
        m1 <- runMaybeT $ f exec hdl v
        case m1 of
            Nothing -> pure m1
            Just _ -> do
                m2 <- runMaybeT $ g exec hdl v
                pure (m1 >> m2)
