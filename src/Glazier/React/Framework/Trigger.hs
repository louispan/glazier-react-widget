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

type Trigger' m a = (J.JSString, J.JSVal -> MaybeT m a)

newtype Triggers m (a :: [Type]) acts = Triggers
    ( Proxy a
    , DL.DList (Trigger' m (Which acts))
    )

-- | identity for 'andBuild'
boring :: Triggers m '[] acts
boring = Triggers (Proxy, mempty)

-- | It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
andTriggers :: Triggers m a1 acts -> Triggers m a2 acts -> Triggers m (AppendUnique a1 a2) acts
andTriggers (Triggers (Proxy, f)) (Triggers (Proxy, g)) = Triggers (Proxy, f <> g)

runTriggers :: Triggers m a acts -> DL.DList (Trigger' m (Which acts))
runTriggers (Triggers (_, ts)) = ts

trigger
    :: (Monad m, UniqueMember a acts)
    => (J.JSString, J.JSVal -> MaybeT m a) -> Triggers m '[a] acts
trigger (evt, f) = Triggers (Proxy, DL.singleton (evt, fmap pick . f))
