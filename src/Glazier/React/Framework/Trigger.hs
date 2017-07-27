{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Diverse
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Attach as F
import qualified JavaScript.Extras as JE

data TriggerAction = TriggerAction J.JSString R.EventTarget

newtype Trigger (a :: [Type]) acts = Trigger
    { getTrigger :: (Proxy a, M.Map J.JSString (TriggerAction -> [Which acts]))
    }

-- | The action types are merged, not appended
instance (a3 ~ AppendUnique a1 a2) =>
         F.Attach (Trigger a1 acts)
                  (Trigger a2 acts)
                  (Trigger a3 acts) where
    Trigger (_, f) +<>+ Trigger (_, f') = Trigger (Proxy, M.unionWith (<>) f f')
    -- | Only run right trigger handler if left trigger handler didn't produce any output
    Trigger (_, f) +<|>+ Trigger (_, f') = Trigger (Proxy, M.unionWith go f f')
        where
          go g g' x = case g x of
                          [] -> g' x
                          y -> y

instance F.AttachId (Trigger '[] acts) where
    aempty = ignore

instance Semigroup (Trigger a acts) where
    Trigger (_, m) <> Trigger (_, m') = Trigger (Proxy, M.unionWith (<>) m m')

ignore :: Trigger '[] acts
ignore = Trigger (Proxy, mempty)

toTrigger :: UniqueMember a acts => J.JSString -> (TriggerAction -> a) -> Trigger '[a] acts
toTrigger n f = Trigger (Proxy, M.singleton n (pure . pick <$> f))

onTrigger :: J.JSString -> J.JSVal -> MaybeT IO TriggerAction
onTrigger n = R.eventHandlerM strictly lazily
  where
    strictly evt = MaybeT . pure $ JE.fromJS evt <&> (R.target . R.parseEvent)
    lazily j = pure $ TriggerAction n j
