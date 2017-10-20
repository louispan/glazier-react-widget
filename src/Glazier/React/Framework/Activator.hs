{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Framework.Activator where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Diverse
import qualified Data.DList as DL
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F

-- | StateT contains the cleanup code if activation failed
-- otherwise the return is the monad to run to commit the activation.
newtype Activator m s' s a c = Activator
    { runActivator ::
            IORef s -- IORef that contains the parent state
            -> (s -> m s') -- how sto get to the child state
            -> F.Handler m s' a c -- externally provided handlers
            -> F.Executor (DL.DList c) -- effectful interpreters
            -- possibly return monadic action to commit the activation
            -- otherwise the StateT contains the action to cleanup
            -> MaybeT (StateT (R.Disposable ()) m) (m ())
    }

-- | identity for 'andActivator'
inert :: Monad m => Activator m s' s (Which '[]) c
inert = Activator $ \_ _ _ _ -> pure (pure ())

-- | It is okay to combine activators the expect the same @a@ action, hence the use of 'AppendUnique'
andActivator
    :: ( Monad m
       , Reinterpret' (AppendUnique a1 a2) a1
       , Reinterpret' (AppendUnique a1 a2) a2
       )
    => Activator m s s' (Which a1) c
    -> Activator m s s' (Which a2) c
    -> Activator m s s' (Which (AppendUnique a1 a2)) c
andActivator (Activator f) (Activator g) = Activator go
  where
    go v this hdl exec =
        liftA2
            (>>)
            (f v this (F.lfilterHandler reinterpret' hdl) exec)
            (g v this (F.lfilterHandler reinterpret' hdl) exec)
