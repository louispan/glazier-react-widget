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
import qualified Glazier.React.Framework.Widget as F

type Activator' s acts cmds = F.Executor' cmds -- effectful interpreters
            -> F.Handler' s acts cmds -- externally provided handlers
            -> TVar s -- TVar that contains the specs
            -> MaybeT (F R.Reactor) (STM ())

newtype Activator s (a :: [Type]) acts (c :: [Type]) cmds =
    Activator ( Proxy a -- required actions from externally provided handlers
              , Proxy c -- required commands from wrapped handlers
              , Activator' s acts cmds)

-- | identity for 'andActivator'
inert :: Activator s '[] acts '[] cmds
inert = Activator (Proxy, Proxy, \_ _ _ -> pure (pure ()))

-- | It is okay to combine activators the expect the same @a@ action, hence the use of 'AppendUnique'
andActivator
    :: Activator s a1 acts c1 cmds
    -> Activator s a2 acts c2 cmds
    -> Activator s (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
andActivator (Activator (_, _, activateDesign)) (Activator (_, _, activateDesign')) =
    Activator ( Proxy
            , Proxy
            , \exec hdl v -> liftA2 (>>) (activateDesign exec hdl v) (activateDesign' exec hdl v)
            )
