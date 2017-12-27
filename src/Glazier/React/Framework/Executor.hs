{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Executor where

import Control.Arrow
import qualified Control.Category as C
import Control.DeepSeq
import Control.Lens
import Data.Diverse.Lens
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Profunctor
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Executor m r x c a b = Executor {
    runExecutor
        :: (DL.DList c -> m (DL.DList x)) -- final transformation
        -> (F.Activator m r, F.Handler m r a b)
    } deriving Functor

-- Using 'F.Handler' instance
instance Functor m => Profunctor (Executor m r x c) where
    dimap f g (Executor exec) = Executor $ fmap (fmap (dimap f g)) exec

-- Using 'F.Handler' instance
instance Functor m => Strong (Executor m r x c) where
    first' (Executor exec) = Executor $ fmap (fmap first') exec
    second' (Executor exec) = Executor $ fmap (fmap second') exec

-- Using 'F.Handler' instance
instance Applicative m => Choice (Executor m r x c) where
    left' (Executor exec) = Executor $ fmap (fmap left') exec
    right' (Executor exec) = Executor $ fmap (fmap right') exec

-- Using 'F.Handler' instance
instance Monad m => C.Category (Executor m r x c) where
    id = Executor $ const (mempty, C.id)
    (Executor exec1) . (Executor exec2) = Executor $ \k ->
        let (act1, hdl1) = exec1 k
            (act2, hdl2) = exec2 k
        in (act1 <> act2, hdl1 C.. hdl2)

-- Using 'F.Handler' instance
instance Monad m => Arrow (Executor m r x c) where
    arr f = rmap f C.id
    first = first'
    second = second'

-- Using 'F.Handler' instance
instance Monad m => ArrowChoice (Executor m r x c) where
    left = left'
    right = right'

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type RefExecutor m v s x c a b = Executor m (IORef v, ReifiedLens' v s) x c a b

-----------------------------------

newtype PExecutor m r x cab = PExecutor
    { runPExecutor :: Executor m r x (P.At0 cab) (P.At1 cab) (P.At2 cab)
    }

type instance P.PNullary (PExecutor m r x) (c, a, b) = Executor m r x c a b

instance Monad m => P.PMEmpty (PExecutor m r x) (Which '[], Which '[], Which '[]) where
    pmempty = Executor $ \_ -> (mempty, P.pmempty)

-- | A friendlier constraint synonym for 'Executor' 'pmappend'.
type PmappendExecutor c1 c2 c3 a1 a2 a3 b1 b2 b3 =
    ( ChooseBetween a1 a2 a3 b1 b2 b3
    , Diversify c1 c3
    , Diversify c2 c3
    , c3 ~ AppendUnique c1 c2
    )

-- | Undecidableinstances!
instance ( Monad m
         , PmappendExecutor c1 c2 c3 a1 a2 a3 b1 b2 b3) =>
         P.PSemigroup (PExecutor m r x)
              (Which c1, Which a1, Which b1)
              (Which c2, Which a2, Which b2)
              (Which c3, Which a3, Which b3) where
    x `pmappend` y = Executor $ \k ->
        let (act1, hdl1) = runExecutor (mapExecutorCommand diversify x) k
            (act2, hdl2) = runExecutor (mapExecutorCommand diversify y) k
        in (act1 <> act2, hdl1 +||+ hdl2)

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
triggerExecutor
    :: (R.MonadReactor x m, NFData a)
    => [F.Trigger a]
    -> RefExecutor m v (DL.DList R.Listener) x a (Which '[]) (Which '[])
triggerExecutor triggers = Executor $ \k -> (F.Activator $ act k, P.pmempty)
  where
    act k (ref, Lens this) = do
        cbs <- traverse {- list of triggers traverse -}
                  (traverse {- tuple traverse -} (\t' ->
                       R.mkCallback t' k) . F.runTrigger) triggers
        R.doModifyIORef' ref (this %~ (`DL.append` DL.fromList cbs))

-- | Variation of 'triggersRefActivator' using a state of @(F.ComponentModel, Many s)@
triggerExecutor'
    :: (R.MonadReactor x m, NFData a, HasItem' (DL.DList R.Listener) s)
    => [F.Trigger a]
    -> RefExecutor m v (F.ComponentPlan, s) x a (Which '[]) (Which '[])
triggerExecutor' = F.viaModel (_2.item') . triggerExecutor

-- | Add a handler so that it is piped before the final transformation to @x@
executeHandler ::
    ( R.MonadReactor x m
    )
    => F.Handler m r c d -> Executor m r x c a b -> Executor m r x d a b
executeHandler (F.Handler hdl) (Executor exec) = Executor $ \k ->
    ( F.Activator $ \env -> (F.runActivator . fst $ exec (k' env k)) env
    , F.Handler $ \env a -> (F.runHandler . snd $ exec (k' env k)) env a
    )
  where
    k' env k cs = fold <$> traverse (hdl env) (DL.toList cs) >>= k

-- | Ignore certain commands
suppressExecutorCommand :: (c -> Maybe c') -> Executor m r x c a b  -> Executor m r x c' a b
suppressExecutorCommand f (Executor exec) = Executor $ \k -> exec (k . foldMap go)
  where
    go x = case f x of
        Nothing -> mempty
        Just x' -> DL.singleton x'

-- | Map a function to the commands
mapExecutorCommand :: (c -> c') -> Executor m r x c a b -> Executor m r x c' a b
mapExecutorCommand f (Executor exec) = Executor $ \k -> exec (k . fmap f)

mapExecutorActivator :: (F.Activator m r -> F.Activator m r) -> Executor m r x c a b -> Executor m r x c a b
mapExecutorActivator f (Executor exec) = Executor $ \k ->
    let (act', hdl') = exec k
    in (f act', hdl')

mapExecutorHandler :: (F.Handler m r a b -> F.Handler m r a' b') -> Executor m r x c a b -> Executor m r x c a' b'
mapExecutorHandler f (Executor exec) = Executor $ \k ->
    let (act', hdl') = exec k
    in (act', f hdl')

------------------------------------------

newtype ExecutorOnModel m v x c a b s = ExecutorOnModel {
    runExecutorOnModel :: RefExecutor m v s x c a b
    }

type instance F.OnModel (ExecutorOnModel m v x c a b) s = RefExecutor m v s x c a b

instance F.ViaModel (ExecutorOnModel m v x c a b) where
    viaModel l (Executor exec) = Executor $ \k ->
        let (act, hdl) = exec k
        in (F.viaModel l act, F.viaModel l hdl)
