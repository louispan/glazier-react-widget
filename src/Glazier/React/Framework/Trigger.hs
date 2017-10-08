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
import Data.Semigroup (Semigroup(..))
import qualified GHCJS.Types as J

type Trigger' m a = (J.JSString, J.JSVal -> MaybeT m a)

newtype Triggers m (a :: [Type]) acts = Triggers
    { runTriggers :: DL.DList (Trigger' m (Which acts))
    }

-- | identity for 'andBuild'
boring :: Triggers m '[] acts
boring = Triggers mempty

-- | It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
andTriggers :: Triggers m a1 acts -> Triggers m a2 acts -> Triggers m (AppendUnique a1 a2) acts
andTriggers (Triggers f) (Triggers g) = Triggers (f <> g)

trigger'
    :: (Monad m, UniqueMember a acts)
    => (J.JSString, J.JSVal -> MaybeT m a) -> Triggers m '[a] acts
trigger' (evt, f) = Triggers (DL.singleton (evt, fmap pick . f))

trigger
    :: (Monad m, Diversify a acts)
    => (J.JSString, J.JSVal -> MaybeT m (Which a)) -> Triggers m a acts
trigger (evt, f) = Triggers (DL.singleton (evt, fmap diversify . f))
