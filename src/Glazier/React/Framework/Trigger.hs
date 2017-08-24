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

newtype Trigger (t :: [Type]) acts = Trigger
    ( Proxy t
    , D.DList (J.JSString, J.JSVal -> MaybeT IO (Which acts))
    )

boring :: Trigger '[] acts
boring = Trigger (Proxy, mempty)

trigger
    :: UniqueMember t acts
    => (J.JSString, J.JSVal -> MaybeT IO t) -> Trigger '[t] acts
trigger (evt, f) = Trigger (Proxy, D.singleton (evt, fmap pick . f))

-- | NB. Unlike @Handler@, it is okay for more than one trigger to results in the same action,
-- hence the use of @AppendUnique@
andTrigger :: Trigger t1 acts -> Trigger t2 acts -> Trigger (AppendUnique t1 t2) acts
andTrigger (Trigger (Proxy, f)) (Trigger (Proxy, g)) = Trigger (Proxy, f <> g)
