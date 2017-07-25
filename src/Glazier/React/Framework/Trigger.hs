{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Applicative
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Kind
import qualified Data.JSString as JS
import Data.Proxy
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Attach as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Gizmo as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

data TriggerAction = TriggerAction JS.JSString R.EventTarget

newtype TriggerPlan = TriggerPlan
    { _triggers :: M.Map JS.JSString (J.Callback (J.JSVal -> IO ()))
    } deriving (G.Generic)

makeClassy ''TriggerPlan

instance R.Dispose TriggerPlan where
    dispose = R.dispose . fmap snd . M.toList . _triggers

toWindowListeners
    :: forall dtls plns.
       (UniqueMember TriggerPlan plns)
    => F.ToWindowListeners dtls plns
toWindowListeners p =
    let xs = p ^. F.plans . item @TriggerPlan . triggers
    in  D.fromList . fmap F.WindowListener . M.toList $ xs

doOnTrigger :: JS.JSString -> J.JSVal -> MaybeT IO TriggerAction
doOnTrigger n = R.eventHandlerM strictly lazily
  where
    strictly evt = MaybeT . pure $ JE.fromJS evt <&> (R.target . R.parseEvent)
    lazily j = pure $ TriggerAction n j

handlersPlan :: J.JSString -> (TriggerAction -> [Which acts]) -> F (R.Maker (Which acts)) TriggerPlan
handlersPlan n f = (TriggerPlan . M.singleton n) <$> R.mkHandler (fmap f <$> doOnTrigger n)

newtype TriggerHandler (a :: [Type]) acts = Trigger
    { runTrigger :: (Proxy a, TriggerAction -> [Which acts])
    }

instance (a3 ~ Append a1 a2) =>
         F.Attach (TriggerHandler a1 acts)
                  (TriggerHandler a2 acts)
                  (TriggerHandler a3 acts) where
    Trigger (pa, f) +<>+ Trigger (pa', f') = Trigger (pa F.+<>+ pa', f <> f')
    -- | Only run right trigger handler if left trigger handler didn't produce any output
    Trigger (pa, f) +<|>+ Trigger (pa', f') = Trigger (pa F.+<>+ pa', \x ->
          case f x of
              [] -> f' x
              y -> y)

instance F.AttachId (TriggerHandler '[] acts) where
    aempty = ignore

ignore :: TriggerHandler '[] acts
ignore = Trigger (Proxy, const [])

onTrigger :: UniqueMember a acts => (TriggerAction -> a) -> TriggerHandler '[a] acts
onTrigger f = Trigger (Proxy, pure . pick <$> f)

triggerWidget
    :: (UniqueMember TriggerPlan plns)
    => J.JSString -> TriggerHandler a acts -> F.Widget '[] '[] '[TriggerPlan] a '[] ols dtls plns v acts cmds
triggerWidget n (Trigger (p, f)) = F.Widget
    ( p
    , Proxy
    , F.Display (mempty, toWindowListeners, Nothing)
    , F.Gizmo
          ( const $ pure nil
          , const nil
          , (single <$> handlersPlan n f)
          , empty))
