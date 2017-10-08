{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.DList as DL
import Data.Kind
import qualified Data.Map.Strict as M
import qualified GHCJS.Types as J
import qualified Glazier.React.Framework.Widget as F

-- type Trigger' a = (J.JSString, J.JSVal -> MaybeT IO (DL.DList (Which a)))

newtype Triggers (a :: [Type]) acts = Triggers
    { runTriggers :: M.Map J.JSString (J.JSVal -> MaybeT IO (DL.DList (Which acts)))
    }

-- | identity for 'andBuild'
boring :: Triggers '[] acts
boring = Triggers mempty

-- | It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
andTriggers :: Triggers a1 acts -> Triggers a2 acts -> Triggers (AppendUnique a1 a2) acts
andTriggers (Triggers f) (Triggers g) = Triggers (M.unionWith (liftA2 (F.catMaybeT)) f g)

trigger'
    :: (UniqueMember a acts)
    => J.JSString -> (J.JSVal -> MaybeT IO a) -> Triggers '[a] acts
trigger' evt f = Triggers (M.singleton evt (fmap (DL.singleton . pick) . f))

trigger
    :: (Diversify a acts)
    => J.JSString -> (J.JSVal -> MaybeT IO (Which a)) -> Triggers a acts
trigger evt f = Triggers (M.singleton evt (fmap (DL.singleton . diversify) . f))
