{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.DList as D
import Data.Kind
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import qualified GHCJS.Types as J

newtype Trigger' a = Trigger' (J.JSString, J.JSVal -> MaybeT IO a)

instance Functor Trigger' where
    fmap f (Trigger' (evt, t)) = Trigger' (evt, fmap f <$> t)

newtype Triggers (a :: [Type]) acts = Triggers
    ( Proxy a
    , D.DList (Trigger' (Which acts))
    )

getTriggers :: Triggers a a -> [Trigger' (Which a)]
getTriggers (Triggers (_, ts)) = D.toList ts

boring :: Triggers '[] acts
boring = Triggers (Proxy, mempty)

expect :: Proxy a -> Triggers '[a] acts
expect _ = Triggers (Proxy, mempty)

trigger
    :: UniqueMember a acts
    => (J.JSString, J.JSVal -> MaybeT IO a) -> Triggers '[a] acts
trigger (evt, f) = Triggers (Proxy, D.singleton $ Trigger' (evt, fmap pick . f))

-- | NB. It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
andTriggers :: Triggers a1 acts -> Triggers a2 acts -> Triggers (AppendUnique a1 a2) acts
andTriggers (Triggers (Proxy, f)) (Triggers (Proxy, g)) = Triggers (Proxy, f <> g)
