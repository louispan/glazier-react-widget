{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Glazier.React.Framework.Activator where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Diverse
import Data.Kind
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F

-- | State contains the cleanup code if activation failed
-- otherwise the return is the monad to run to commit the activation.
type Activator' m s a c
    = F.Executor' c -- effectful interpreters
    -> F.Handler' m s a c -- externally provided handlers
    -> IORef s -- IORef that contains the specs
    -> MaybeT (StateT (R.Disposable ()) m) (m ())

-- | If @commit@ is Nothing, then @cleanup@ must be run to cleanup handlers created.
newtype Activator m s (a :: [Type]) acts cmds = Activator
    { runActivator :: Activator' m s acts cmds
    }

-- | identity for 'andActivator'
inert :: Monad m => Activator m s '[] acts cmds
inert = Activator $ \_ _ _ -> pure (pure ())

-- | It is okay to combine activators the expect the same @a@ action, hence the use of 'AppendUnique'
andActivator
    :: Monad m
    => Activator m s a1 acts cmds
    -> Activator m s a2 acts cmds
    -> Activator m s (AppendUnique a1 a2) acts cmds
andActivator (Activator f) (Activator g) = Activator go
  where
    go exec hdl v = liftA2 (>>) (f exec hdl v) (g exec hdl v)
