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
import qualified Glazier.React.Framework.Firsts as F
import qualified JavaScript.Extras as JE

data TriggerAction = TriggerAction J.JSString R.EventTarget

newtype Trigger (a :: [Type]) acts = Trigger
    (Proxy a, M.Map J.JSString (TriggerAction -> [Which acts]))

-- | The action types are merged, not appended
andTrigger :: Trigger a1 acts -> Trigger a2 acts -> Trigger (AppendUnique a1 a2) acts
andTrigger (Trigger (_, f)) (Trigger (_, f') )= Trigger (Proxy, M.unionWith (<>) f f')

-- | Only run right trigger handler if left trigger handler didn't produce any output
orTrigger :: Trigger a1 acts -> Trigger a2 acts -> Trigger (AppendUnique a1 a2) acts
orTrigger (Trigger (_, f)) (Trigger (_, f')) = Trigger (Proxy, M.unionWith go f f')
  where
    go g g' x = case g x of
                    [] -> g' x
                    y -> y

instance Semigroup (Trigger a acts) where
    Trigger (_, m) <> Trigger (_, m') = Trigger (Proxy, M.unionWith (<>) m m')

instance F.Firsts (Trigger a acts) where
    Trigger (_, m) <<|>> Trigger (_, m') = Trigger (Proxy, M.unionWith go m m')
      where
        go g g' x = case g x of
                        [] -> g' x
                        y -> y

ignore :: Trigger '[] acts
ignore = Trigger (Proxy, mempty)

trigger :: UniqueMember a acts => J.JSString -> (TriggerAction -> a) -> Trigger '[a] acts
trigger n f = Trigger (Proxy, M.singleton n (pure . pick <$> f))

onEvent :: J.JSString -> J.JSVal -> MaybeT IO TriggerAction
onEvent n = R.eventHandlerM strictly lazily
  where
    strictly evt = MaybeT . pure $ JE.fromJS evt <&> (R.target . R.parseEvent)
    lazily j = pure $ TriggerAction n j
