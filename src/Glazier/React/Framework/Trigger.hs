{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.DList as D
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup (Semigroup)
import GHC.Exts (Any, coerce)
import qualified GHCJS.Types as J
import qualified Glazier.React.Framework.Widget as F
import Unsafe.Coerce

type ActionHandler a specs = TMVar (F.Design specs) -> a -> STM ()

newtype TriggerHandler a specs = TriggerHandler (J.JSVal -> MaybeT IO a, ActionHandler a specs)

newtype Trigger (a :: [Type]) specs = Trigger (M.Map J.JSString (ActionHandler Any specs))

-- Inferred role is phantom which is incorrect
-- representational means:
-- @
-- Coercible '[Int] '[IntLike] => Coercible (Trigger '[Int]) (Trigger '[IntLike])
-- @
type role Trigger representational representational

boring :: Trigger '[] specs
boring = Trigger M.empty

toActionHandlers :: Trigger a specs -> Many hs

-- instance Semigroup (ActionHandler a) where
--     ActionHandler (fire, f) <> ActionHandler (_, g) = ActionHandler (fire, \d a -> f d a >> g d a)

-- newtype Trigger (t :: [Type]) trigs (a :: [Type]) acts =
--     Trigger ( Proxy t
--             , Proxy a
--             , M.Map J.JSString
--               ( J.JSVal -> MaybeT IO (Which trigs) -- triggers
--               , Which trigs -> MaybeT IO (D.DList (Which acts)))  -- trigger handlers
--             )

-- andTrigger
--     :: Trigger t1 trigs a1 acts
--     -> Trigger t2 trigs a2 acts
--     -> Trigger (AppendUnique t1 t2) trigs (Append a1 a2) acts
-- andTrigger (Trigger (_, _, x)) (Trigger (_, _, x')) =
--     Trigger ( Proxy
--             , Proxy
--             , M.unionWith combine x x')
--   where
--     combine (f, g) (_, g') =
--         ( f -- only left trigger is kept
--         , liftA2 combine' g g') -- all handlers are combined
--     combine' f g = MaybeT $ do
--         a <- runMaybeT f
--         b <- runMaybeT g
--         case (a, b) of
--             (Nothing, b') -> pure b'
--             (a', Nothing) -> pure a'
--             (Just a', Just b') -> pure (Just (a' <> b'))

-- orTrigger
--     :: Trigger t1 trigs a1 acts
--     -> Trigger t2 trigs a2 acts
--     -> Trigger (AppendUnique t1 t2) trigs (AppendUnique a1 a2) acts
-- orTrigger (Trigger (_, _, x)) (Trigger (_, _, x')) =
--     Trigger ( Proxy
--             , Proxy
--             , M.unionWith combine x x')
--   where
--     combine (f, g) (_, g') =
--         ( f -- only left trigger is kept
--         , liftA2 combine' g g') -- handlers are combined only if existing handler produce nothing
--     combine' g g' = do
--         r <- g
--         case D.toList r of
--             [] -> g'
--             y -> pure $ D.fromList y

-- instance Semigroup (Trigger t trigs a acts) where
--     (Trigger (_, _, x)) <> (Trigger (_, _, x')) =
--         Trigger ( Proxy
--                 , Proxy
--                 , M.unionWith combine x x')
--       where
--         combine (f, g) (_, g') =
--             ( f -- only left trigger is kept
--             , liftA2 combine' g g') -- all handlers are combined
--         combine' f g = MaybeT $ do
--             a <- runMaybeT f
--             b <- runMaybeT g
--             case (a, b) of
--                 (Nothing, b') -> pure b'
--                 (a', Nothing) -> pure a'
--                 (Just a', Just b') -> pure (Just (a' <> b'))

-- instance F.Firsts (Trigger t trigs a acts) where
--     (Trigger (_, _, x)) <<|>> (Trigger (_, _, x')) =
--         Trigger ( Proxy
--                 , Proxy
--                 , M.unionWith combine x x')
--       where
--         combine (f, g) (_, g') =
--             ( f -- only left trigger is kept
--             , liftA2 combine' g g') -- handlers are combined only if existing handler produce nothing
--         combine' g g' = do
--             r <- g
--             case D.toList r of
--                 [] -> g'
--                 y -> pure $ D.fromList y

-- -- | Identity for 'andTrigger' and 'orTrigger'
-- boring :: Trigger '[] trigs '[] acts
-- boring = Trigger (Proxy, Proxy, mempty)

-- -- | The (J.JSVal -> MaybeT IO t) is assumed the same for all
-- -- Triggers of type t.
-- trigger
--     :: ( UniqueMember a acts
--        , UniqueMember t trigs)
--     => J.JSString
--     -> (J.JSVal -> MaybeT IO t)
--     -> (t -> MaybeT IO a)
--     -> Trigger '[t] trigs '[a] acts
-- trigger n f g = Trigger (Proxy, Proxy, M.singleton n (fmap pick <$> f, g'))
--   where
--     g' x = case trial x of
--                Left _ -> pure mempty
--                Right x' -> D.singleton . pick <$> g x'

-- -- | Use to indicate that we are expecting an action fired
-- -- from other widgets instead directly from event listeners.
-- expect
--     :: UniqueMember a acts -- Has redundant constraint warning, but I want to ensure that a is in acts.
--     => Proxy a -> Trigger '[] trigs '[a] acts
-- expect _ = Trigger (Proxy, Proxy, mempty)
