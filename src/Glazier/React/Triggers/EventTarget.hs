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

module Glazier.React.Triggers.EventTarget where

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
import qualified Glazier.React.Widget as R
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
    => R.ToWindowListeners dtls plns
toWindowListeners p =
    let xs = p ^. R.plans . item @TriggerPlan . triggers
    in  D.fromList . fmap R.WindowListener . M.toList $ xs

doOnTrigger :: JS.JSString -> J.JSVal -> MaybeT IO TriggerAction
doOnTrigger n = R.eventHandlerM strictly lazily
  where
    strictly evt = MaybeT . pure $ JE.fromJS evt <&> (R.target . R.parseEvent)
    lazily j = pure $ TriggerAction n j

handlersPlan :: J.JSString -> (TriggerAction -> [Which acts]) -> F (R.Maker (Which acts)) TriggerPlan
handlersPlan n f = (TriggerPlan . M.singleton n) <$> R.mkHandler (fmap f <$> doOnTrigger n)

type TriggerHandler (a ::[Type]) acts = (Proxy a, TriggerAction -> [Which acts])

onTrigger :: UniqueMember a acts => (TriggerAction -> a) -> TriggerHandler '[a] acts
onTrigger f = (Proxy, pure . pick <$> f)

combineTriggerHandler :: TriggerHandler a acts -> TriggerHandler b acts -> TriggerHandler (Append a b) acts
combineTriggerHandler (_, f) (_, g) = (Proxy, f <> g)

-- | Only run right trigger handler if left trigger handler didn't produce any output
combineTriggerHandler' :: TriggerHandler a acts -> TriggerHandler b acts -> TriggerHandler (Append a b) acts
combineTriggerHandler' (_, f) (_, g) =
    ( Proxy
    , \x ->
          case f x of
              [] -> g x
              y -> y)

triggerWidget
    :: (UniqueMember TriggerPlan plns)
    => J.JSString -> TriggerHandler a acts -> R.Widget '[] '[] '[TriggerPlan] a '[] ols dtls plns v acts cmds
triggerWidget n (p, f) =
    ( p
    , Proxy
    , R.Display (mempty, toWindowListeners, Nothing)
    , R.Gizmo
          ( const $ pure nil
          , const nil
          , (single <$> handlersPlan n f)
          , empty))
