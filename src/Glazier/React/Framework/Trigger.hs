{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.DList as DL
import Data.Kind
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import qualified GHCJS.Types as J

type Trigger' a = (J.JSString, J.JSVal -> MaybeT IO a)

-- instance Functor Trigger' where
--     fmap f (Trigger' (evt, t)) = Trigger' (evt, fmap f <$> t)

newtype Triggers (t :: [Type]) acts = Triggers
    ( Proxy t
    , DL.DList (Trigger' (Which acts))
    )

instance Semigroup (Triggers '[] acts) where
    _ <> _ = Triggers (Proxy, mempty)

instance Monoid (Triggers '[] acts) where
    mempty = Triggers (Proxy, mempty)
    mappend = (<>)

-- | mempty is also identity for 'andBuild'
-- NB. It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
andTriggers :: Triggers t1 acts -> Triggers t2 acts -> Triggers (AppendUnique t1 t2) acts
andTriggers (Triggers (Proxy, f)) (Triggers (Proxy, g)) = Triggers (Proxy, f <> g)

getTriggers :: Triggers t acts -> DL.DList (Trigger' (Which acts))
getTriggers (Triggers (_, ts)) = ts

-- expect :: Proxy a -> Triggers '[a] acts
-- expect _ = Triggers (Proxy, mempty)

trigger
    :: UniqueMember t acts
    => (J.JSString, J.JSVal -> MaybeT IO t) -> Triggers '[t] acts
trigger (evt, f) = Triggers (Proxy, DL.singleton (evt, fmap pick . f))
