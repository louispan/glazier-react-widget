{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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

import Control.DeepSeq
import Control.Lens
import Data.Diverse.Lens
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
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
        :: r
        -> (DL.DList c -> m (DL.DList x)) -- final transformation
        -> (F.Handler m r a b, F.Activator m r)
    }

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type RefExecutor m v s x c a b = Executor m (IORef v, ReifiedLens' v s) x c a b

-----------------------------------

newtype PExecutor m r x cab = PExecutor
    { runPExecutor :: Executor m r x (P.At0 cab) (P.At1 cab) (P.At2 cab)
    }

type instance P.PNullary (PExecutor m r x) (c, a, b) = Executor m r x c a b

instance Monad m => P.PMEmpty (PExecutor m r x) (Which '[], Which '[], Which '[]) where
    pmempty = Executor $ \_ _ -> (P.pmempty, mempty)

-- | Undecidableinstances!
instance ( Monad m
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         , c3 ~ AppendUnique c1 c2
         , Diversify c1 c3
         , Diversify c2 c3
         ) =>
         P.PSemigroup (PExecutor m r x)
              (Which c1, Which a1, Which b1)
              (Which c2, Which a2, Which b2)
              (Which c3, Which a3, Which b3) where
    x `pmappend` y = Executor $ \env k ->
        let (hdl1, act1) = runExecutor (mapExecutor diversify x) env k
            (hdl2, act2) = runExecutor (mapExecutor diversify y) env k
        in (hdl1 +||+ hdl2, act1 <> act2)

-- | Ignore certain commands
suppressExecutor :: (c -> Maybe c') -> Executor m r x c a b  -> Executor m r x c' a b
suppressExecutor f (Executor exec) = Executor $ \env k -> exec env (k . foldMap go)
  where
    go x = case f x of
        Nothing -> mempty
        Just x' -> DL.singleton x'

-- | Map a function to the commands
mapExecutor :: (c -> c') -> Executor m r x c a b -> Executor m r x c' a b
mapExecutor f (Executor exec) = Executor $ \env k -> exec env (k . fmap f)

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
addTriggers
    :: (R.MonadReactor x m, NFData a)
    => [F.Trigger a]
    -> RefExecutor m v (DL.DList R.Listener) x a (Which '[]) (Which '[])
addTriggers triggers = Executor $ \_ k -> (P.pmempty, F.Activator $ act k)
  where
    act k (ref, Lens this) = do
        cbs <- traverse {- list of triggers traverse -}
                  (traverse {- tuple traverse -} (\t' ->
                       R.mkCallback t' k) . F.runTrigger) triggers
        R.doModifyIORef' ref (this %~ (`DL.append` DL.fromList cbs))

-- | Variation of 'triggersRefActivator' using a state of @(F.ComponentModel, Many s)@
addTriggers'
    :: (R.MonadReactor x m, NFData a, HasItem' (DL.DList R.Listener) s)
    => [F.Trigger a]
    -> RefExecutor m v (F.ComponentModel, s) x a (Which '[]) (Which '[])
addTriggers' = F.viaModel (_2.item') . addTriggers

-- | Add a handler so that it is piped before the final transformation to @x@
addHandler ::
    ( R.MonadReactor x m
    )
    => F.Handler m r c d -> Executor m r x c a b -> Executor m r x d a b
addHandler (F.Handler hdl) (Executor exec) = Executor $ \env k ->
    let (hdl', act') = exec env (k' env k)
    in (hdl', act')
  where
    k' env k cs = fold <$> traverse (hdl env) (DL.toList cs) >>= k

mapExecutorHandler :: (F.Handler m r a b -> F.Handler m r a' b') -> Executor m r x c a b -> Executor m r x c a' b'
mapExecutorHandler f (Executor exec) = Executor $ \env k ->
    let (hdl', act') = exec env k
    in (f hdl', act')

------------------------------------------

newtype ExecutorModeller m v x c a b s = ExecutorModeller {
    runExecutorModeller :: RefExecutor m v s x c a b
    }

type instance F.Modeller (ExecutorModeller m v x c a b) s = RefExecutor m v s x c a b

instance F.ViaModel (ExecutorModeller m v x c a b) where
    viaModel l (Executor exec) = Executor $ \(ref, Lens this) k ->
        let (hdl, act) = exec (ref, Lens (this.l)) k
        in (F.viaModel l hdl, F.viaModel l act)
