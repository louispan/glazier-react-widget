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
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Widget as F

newtype Builder (r :: [Type]) reqs (s :: [Type]) specs (ba :: [Type]) acts (bc :: [Type]) cmds =
    Builder ( Proxy ba -- required actions from externally provided handlers
            , Proxy bc -- required commands from wrapped handlers
            , Many specs -> STM (Many r) -- from specifications
            , Many reqs -> F R.Reactor (F.Inactive (Many s)) -- make specifications
            , F.Executor' cmds -- effectful interpreters
            -> F.Handler' (F.Design specs) acts cmds -- externally provided handlers
            -> TVar (F.Inactive (F.Design specs))
            -> TVar (F.Design specs) -- activate design
            )

instance Semigroup (Builder '[] reqs '[] specs '[] acts '[] cmds) where
    _ <> _ = Builder (Proxy, Proxy, const $ pure nil, \_ _ -> const $ pure nil)

instance Monoid (Builder '[] reqs '[] specs '[] acts '[] cmds) where
    mempty = Builder (Proxy, Proxy, const $ pure nil, \_ _  -> const $ pure nil)
    mappend = (<>)

-- | mempty is also identity for 'andBuild'
-- It is okay to combine builders the expect the same @ba@ action, hence the use of 'AppendUnique'
andBuilder
    :: Builder r1 reqs s1 specs ba1 acts bc1 cmds
    -> Builder r2 reqs s2 specs ba2 acts bc2 cmds
    -> Builder (Append r1 r2) reqs (Append s1 s2) specs
                 (AppendUnique ba1 ba2) acts (AppendUnique bc1 bc2) cmds
andBuilder (Builder (_, _, fromSpec, mkSpec)) (Builder (_, _, fromSpec', mkSpec')) =
    Builder ( Proxy
            , Proxy
            , \d -> (/./) <$> fromSpec d <*> fromSpec' d
            , \hdl exec rs -> (/./) <$> mkSpec hdl exec rs <*> mkSpec' hdl exec rs
            )

-- | Add a type @x@ into the factory
build
    :: (UniqueMember x reqs, UniqueMember x specs)
    => Proxy x -> Builder '[x] reqs '[x] specs '[] acts '[] cmds
build _ = Builder ( Proxy
                  , Proxy
                  , pure . single . fetch
                  , \_ _ -> pure . single . fetch
                  )
