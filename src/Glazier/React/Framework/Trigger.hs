{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.DList as D
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React.Framework.Firsts as F

newtype TriggerAction t = TriggerAction t

newtype Trigger (t :: [Type]) trigs (a :: [Type]) acts =
    Trigger ( Proxy t
            , Proxy a
            , M.Map J.JSString
              ( J.JSVal -> MaybeT IO (Which trigs) -- triggers
              , Which trigs -> D.DList (Which acts))  -- trigger handlers
            )

andTrigger
    :: Trigger t1 trigs a1 acts
    -> Trigger t2 trigs a2 acts
    -> Trigger (AppendUnique t1 t2) trigs (AppendUnique a1 a2) acts
andTrigger (Trigger (_, _, x)) (Trigger (_, _, x')) =
    Trigger ( Proxy
            , Proxy
            , M.unionWith combine x x')
  where
    combine (f, g) (_, g') =
        ( f -- only left trigger is kept
        , g <> g') -- all handlers are combined

orTrigger
    :: Trigger t1 trigs a1 acts
    -> Trigger t2 trigs a2 acts
    -> Trigger (AppendUnique t1 t2) trigs (AppendUnique a1 a2) acts
orTrigger (Trigger (_, _, x)) (Trigger (_, _, x')) =
    Trigger ( Proxy
            , Proxy
            , M.unionWith combine x x')
  where
    combine (f, g) (_, g') =
        ( f -- only left trigger is kept
        , combine' g g') -- handlers are combined only if existing handler produce nothing
    combine' g g' a = case D.toList (g a) of
                         [] -> g' a
                         y -> D.fromList y

instance Semigroup (Trigger t trigs a acts) where
    (Trigger (_, _, x)) <> (Trigger (_, _, x')) =
        Trigger ( Proxy
                , Proxy
                , M.unionWith combine x x')
      where
        combine (f, g) (_, g') =
            ( f -- only left trigger is kept
            , g <> g') -- all handlers are combined

instance F.Firsts (Trigger t trigs a acts) where
    (Trigger (_, _, x)) <<|>> (Trigger (_, _, x')) =
        Trigger ( Proxy
                , Proxy
                , M.unionWith combine x x')
      where
        combine (f, g) (_, g') =
            ( f -- only left trigger is kept
            , combine' g g') -- handlers are combined only if existing handler produce nothing
        combine' g g' a = case D.toList (g a) of
                             [] -> g' a
                             y -> D.fromList y

ignore :: Trigger '[] trigs '[] acts
ignore = Trigger (Proxy, Proxy, mempty)

trigger
    :: ( UniqueMember a acts
       , UniqueMember t trigs)
    => J.JSString
    -> (J.JSVal -> MaybeT IO t)
    -> (t -> a)
    -> Trigger '[t] trigs '[a] acts
trigger n f g = Trigger (Proxy, Proxy, M.singleton n (fmap pick <$> f, g'))
  where
    g' x = case trial x of
               Left _ -> mempty
               Right x' -> D.singleton . pick $ g x'

-- onEvent :: J.JSString -> J.JSVal -> MaybeT IO TriggerAction
-- onEvent n = R.eventHandlerM strictly lazily
--   where
--     strictly evt = MaybeT . pure $ JE.fromJS evt <&> (R.target . R.parseEvent)
--     lazily j = pure $ TriggerAction n j
