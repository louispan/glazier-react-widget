{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Activator where

import Control.DeepSeq
import Control.Lens
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Parameterized.Data.Monoid as P

------------------------------------------

newtype Activator m r x c = Activator
    { runActivator :: F.Executor m c (DL.DList x) -- final transformation)
                   -> r -- Handler env
                   -> m () -- return the monadic action to commit the activation
    }

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type RefActivator m v s x c = Activator m (IORef v, ReifiedLens' v s) x c

instance Monad m => Semigroup (Activator m r x c) where
    (Activator f) <> (Activator g) = Activator $ \exec env ->
        f exec env >>
        g exec env

instance Monad m => Monoid (Activator m r x c) where
    mempty = Activator $ \_ _ -> pure ()
    mappend = (<>)

------------------------------------------

type instance P.PNullary (Activator m r x) c = Activator m r x c

instance Applicative m => P.PMEmpty (Activator m r x) (Which '[]) where
    pmempty = Activator $ \_ _ -> pure ()

instance ( Monad m
         , Reinterpret' c3 c1
         , Reinterpret' c3 c2
         , c3 ~ AppendUnique c1 c2
         ) => P.PSemigroup (Activator m r x) (Which c1) (Which c2) (Which c3) where
    (Activator f) `pmappend` (Activator g) = Activator $ \exec env ->
        f (F.suppressExecutor reinterpret' exec) env >>
        g (F.suppressExecutor reinterpret' exec) env

------------------------------------------

-- | Ignore certain commands
suppressActivator :: (c -> Maybe c') ->  Activator m r x c -> Activator m r x c'
suppressActivator f (Activator act) = Activator $ \exec env -> act (F.suppressExecutor f exec) env

-- | Map a function to the commands accepted by the Executor
instance Functor m => Functor (Activator m r x) where
    fmap f (Activator act) = Activator $ \exec env -> act (F.contramapExecutor f exec) env

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
triggersRefActivator
    :: (R.MonadReactor x m, NFData a)
    => [F.Trigger a]
    -> RefActivator m v (DL.DList R.Listener) x a
triggersRefActivator triggers = Activator $ \(F.Executor exec) (ref, Lens this) -> do
    cbs <- traverse {- list of triggers traverse -}
              (traverse {- tuple traverse -} (\t' ->
                   R.mkCallback t' exec) . F.runTrigger) triggers
    R.doModifyIORef' ref (this %~ (`DL.append` DL.fromList cbs))
  where
    -- go :: (a -> m (DL.DList x)) -> DL.DList a -> m (DL.DList x)
    -- go exec as = fold <$> traverse exec (DL.toList as)

-- | Variation of 'triggersRefActivator' using a state of @(F.ComponentModel, Many s)@
triggersRefActivator'
    :: (R.MonadReactor x m, NFData a, HasItem' (DL.DList R.Listener) s)
    => [F.Trigger a]
    -> RefActivator m v (F.ComponentModel, s) x a
triggersRefActivator' = F.viaModel (_2.item') . triggersRefActivator

-- | Add a handler so that it is piped before the input 'Executor' in an 'Activator'
addHandler ::
    ( R.MonadReactor x m
    )
    => F.Handler m r a b -> Activator m r x a -> Activator m r x b
addHandler (F.Handler hdl) (Activator act) = Activator $ \(F.Executor exec) env ->
    act (F.Executor $ \as -> fold <$> traverse (hdl env) (DL.toList as) >>= exec) env

------------------------------------------

newtype ActivatorModeller m v x c s = ActivatorModeller { runActivatorModeller :: RefActivator m v s x c }

type instance F.Modeller (ActivatorModeller m v x c) s = RefActivator m v s x c

instance F.ViaModel (ActivatorModeller m v a b) where
    viaModel l (Activator f) = Activator $ \exec (ref, Lens this) ->
        f exec (ref, Lens (this.l))
