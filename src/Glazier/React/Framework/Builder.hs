{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Builder where

import Control.Concurrent.STM
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
-- import Data.Semigroup (Semigroup(..))
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Widget as F

newtype Builder (r :: [Type]) reqs (s :: [Type]) specs (a :: [Type]) acts (c :: [Type]) cmds =
    Builder ( Proxy a -- required actions from externally provided handlers
            , Proxy c -- required commands from wrapped handlers
            , Many specs -> STM (Many r) -- from specifications
            , Many reqs -> F R.Reactor (Many s) -- make specifications
            , F.Executor' cmds -- effectful interpreters
            -> F.Handler' (F.Design specs) acts cmds -- externally provided handlers
            -> TVar (F.Design specs) -- TVar that contains the specs
            -> MaybeT (F R.Reactor) ()
            )

-- | identity for 'andBuild'
idle :: Builder '[] reqs '[] specs '[] acts '[] cmds
idle = Builder (Proxy, Proxy, const $ pure nil, const $ pure nil, \_ _ _ -> pure ())

-- | It is okay to combine builders the expect the same @ba@ action, hence the use of 'AppendUnique'
andBuilder
    :: Builder r1 reqs s1 specs a1 acts c1 cmds
    -> Builder r2 reqs s2 specs a2 acts c2 cmds
    -> Builder (Append r1 r2) reqs (Append s1 s2) specs
                 (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
andBuilder (Builder (_, _, fromSpec, mkSpec, activateDesign)) (Builder (_, _, fromSpec', mkSpec', activateDesign')) =
    Builder ( Proxy
            , Proxy
            , \s -> (/./) <$> fromSpec s <*> fromSpec' s
            , \r -> (/./) <$> mkSpec r <*> mkSpec' r
            , \exec hdl v -> activateDesign exec hdl v >> activateDesign' exec hdl v
            )

-- | Add a type @x@ into the factory
build
    :: (UniqueMember x reqs, UniqueMember x specs)
    => Proxy x -> Builder '[x] reqs '[x] specs '[] acts '[] cmds
build _ = Builder ( Proxy
                  , Proxy
                  , pure . single . fetch
                  , pure . single . fetch
                  , \_ _ _ -> pure ()
                  )
