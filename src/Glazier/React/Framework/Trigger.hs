{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Applicative
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
              , Which trigs -> MaybeT IO (D.DList (Which acts)))  -- trigger handlers
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
        , liftA2 combine' g g') -- all handlers are combined
    combine' f g = (liftA2 (<>) f g) <|> f <|> g

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
        , liftA2 combine' g g') -- handlers are combined only if existing handler produce nothing
    combine' g g' = do
        r <- g
        case D.toList r of
            [] -> g'
            y -> pure $ D.fromList y

instance Semigroup (Trigger t trigs a acts) where
    (Trigger (_, _, x)) <> (Trigger (_, _, x')) =
        Trigger ( Proxy
                , Proxy
                , M.unionWith combine x x')
      where
        combine (f, g) (_, g') =
            ( f -- only left trigger is kept
            , liftA2 combine' g g') -- all handlers are combined
        combine' f g = (liftA2 (<>) f g) <|> f <|> g

instance F.Firsts (Trigger t trigs a acts) where
    (Trigger (_, _, x)) <<|>> (Trigger (_, _, x')) =
        Trigger ( Proxy
                , Proxy
                , M.unionWith combine x x')
      where
        combine (f, g) (_, g') =
            ( f -- only left trigger is kept
            , liftA2 combine' g g') -- handlers are combined only if existing handler produce nothing
        combine' g g' = do
            r <- g
            case D.toList r of
                [] -> g'
                y -> pure $ D.fromList y

-- | Identity for 'andTrigger' and 'orTrigger'
boring :: Trigger '[] trigs '[] acts
boring = Trigger (Proxy, Proxy, mempty)

trigger
    :: ( UniqueMember a acts
       , UniqueMember t trigs)
    => J.JSString
    -> (J.JSVal -> MaybeT IO t)
    -> (t -> MaybeT IO a)
    -> Trigger '[t] trigs '[a] acts
trigger n f g = Trigger (Proxy, Proxy, M.singleton n (fmap pick <$> f, g'))
  where
    g' x = case trial x of
               Left _ -> pure mempty
               Right x' -> D.singleton . pick <$> g x'
