{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Executor where

import Control.DeepSeq
import Control.Lens
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.Generics.Product
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.IsReader as F
import qualified Glazier.React.Framework.Object as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

-- | A reader with specialized executing environment
newtype Executor m x c a = Executor { runExecutor :: (DL.DList c -> m (DL.DList x)) -> a }
    deriving Functor

instance Applicative (Executor m x c) where
    pure r = Executor $ const r
    (Executor exec1) <*> (Executor exec2) = Executor $ \k -> exec1 k (exec2 k)

instance Monad (Executor m x c) where
    (Executor exec) >>= f = Executor $ \k -> runExecutor (f (exec k)) k

suppressExecutor :: (c -> Maybe c') -> Executor m x c a -> Executor m x c' a
suppressExecutor f (Executor exec) = Executor $ \k -> exec (k . foldMap go)
  where
    go c = maybe mempty DL.singleton (f c)

withExecutor :: (c -> c') -> Executor m x c a -> Executor m x c' a
withExecutor f (Executor exec) = Executor $ \k -> exec (k . fmap f)

-- | Type restricted version of 'pure' where the @c@ is set to @Which '[]@
execute :: a -> Executor m x (Which '[]) a
execute = pure

------------------------------------------------------

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
-- Using @AllowAmbiguousTypes@ instead of @Proxy@
triggers :: forall t m v s x a.
    ( R.MonadReactor x m
    , NFData a
    , HasItemTag' t [R.Listener] s
    )
    => [F.Trigger a]
    -> ExObjActivator m v (F.ComponentPlan x m, s) x a
triggers ts = Executor $ \k -> F.Activator $ act k
  where
    act k (F.Object ref (Lens this)) = do
        cbs <- traverse {- list of triggers traverse -}
                  (traverse {- tuple traverse -} (`R.mkCallback` k) . F.runTrigger) ts
        let cbs' = fmap snd <$> cbs
            ds = foldMap (fst . snd) cbs
        R.doModifyIORef' ref $ \obj ->
            obj & this._2.itemTag' @t %~ (cbs' <>)
                & this._1.field @"disposeOnRemoved" %~ (<> ds)

-- | Use the given handler to transform the Executor's environment
handleBeforeExecuting :: forall a b c d m x r y.
    ( Monad m
    , Injected a c b d
    )
    => r
    -> F.Handler m r (Which a) (Which b)
    -> Executor m x (Which c) y
    -> Executor m x (Which d) y
handleBeforeExecuting env hdl (Executor exec) = Executor $ \k ->
    let F.Handler hdl' = injected @_ @_ @c hdl
        k' cs = (fold <$> traverse (hdl' env) (DL.toList cs)) >>= k
    in exec k'

-- | Use left executor's handler to tranform the right Executor's environment
-- Ie. replace the @a1@ in @c2@ with @b1@ (this is @c3@), but now we also need @c1@ in the environment.
controls ::
    ( R.MonadReactor x m
    , c4 ~ AppendUnique c1 c3
    , Injected a1 c2 b1 c3
    , Diversify c1 c4
    , Diversify c3 c4
    , F.IsReader r y
    )
    => Executor m x (Which c1) (F.Handler m r (Which a1) (Which b1)) -- handler
    -> Executor m x (Which c2) y
    -> Executor m x (Which c4) y
controls exec1 exec2 = Executor $ \k ->
    let Executor exec1' = withExecutor diversify exec1
        hdl1 = exec1' k
    in F.fromReader $ \env ->
        let exec2' = handleBeforeExecuting env hdl1 exec2
            Executor exec2'' = withExecutor diversify exec2'
            y = exec2'' k
        in (F.toReader y) env

------------------------------------------------------

newtype PExHandler m r x zab = PExHandler
    { runPExHandler :: Executor m x (P.At0 zab) (F.Handler m r (P.At1 zab) (P.At2 zab))
    }

type instance P.PNullary (PExHandler m r x) (z, a, b) = Executor m x z (F.Handler m r a b)

instance Applicative m => P.PMEmpty (PExHandler m r x) (Which '[], Which '[], Which '[]) where
    pmempty = Executor $ const P.pmempty

-- | A friendlier constraint synonym for 'Executor' 'pmappend'.
type PmappendExecutor z1 z2 z3 =
    ( Diversify z1 z3
    , Diversify z2 z3
    , z3 ~ AppendUnique z1 z2
    )

-- | Undecidableinstances!
instance ( Monad m
         , PmappendExecutor z1 z2 z3
         , ChooseBetween a1 a2 a3 b1 b2 b3) =>
         P.PSemigroup (PExHandler m r x)
              (Which z1, Which a1, Which b1)
              (Which z2, Which a2, Which b2)
              (Which z3, Which a3, Which b3) where
    x `pmappend` y = Executor $ \k ->
        let hdl1 = runExecutor (withExecutor diversify x) k
            hdl2 = runExecutor (withExecutor diversify y) k
        in (hdl1 +||+ hdl2)

------------------------------------------------------

type ExHandler m r x z a b = Executor m x z (F.Handler m r a b)
type ExObjHandler m v s x z a b = Executor m x z (F.ObjHandler m v s a b)

newtype ExObjHandlerOnModel m v x z a b s = ExObjHandlerOnModel {
    runExObjHandlerOnModel :: ExObjHandler m v s x z a b
    }

type instance F.OnModel (ExObjHandlerOnModel m v x z a b) s = ExObjHandler m v s x z a b

instance F.ViaModel (ExObjHandlerOnModel m v x z a b) where
    viaModel l (Executor exec) = Executor $ \k ->
        let hdl = exec k
        in F.viaModel l hdl


------------------------------------------------------

newtype PExActivator m r x y = PExActivator
    { runPExActivator :: Executor m x y (F.Activator m r)
    }

type instance P.PNullary (PExActivator m r x) y = Executor m x y (F.Activator m r)

instance Monad m => P.PMEmpty (PExActivator m r x) (Which '[]) where
    pmempty = Executor $ const mempty

-- | Undecidableinstances!
instance ( Monad m
         , PmappendExecutor y1 y2 y3) =>
         P.PSemigroup (PExActivator m r x)
              (Which y1)
              (Which y2)
              (Which y3) where
    x `pmappend` y = Executor $ \k ->
        let act1 = runExecutor (withExecutor diversify x) k
            act2 = runExecutor (withExecutor diversify y) k
        in (act1 <> act2)

------------------------------------------------------

type ExActivator m r x y = Executor m x y (F.Activator m r)
type ExObjActivator m v s x y = Executor m x y (F.ObjActivator m v s)

newtype ExObjActivatorOnModel m v x y s = ExObjActivatorOnModel {
    runExObjActivatorOnModel :: ExObjActivator m v s x y
    }

type instance F.OnModel (ExObjActivatorOnModel m v x y) s = ExObjActivator m v s x y

instance F.ViaModel (ExObjActivatorOnModel m v x y) where
    viaModel l (Executor exec) = Executor $ \k ->
        let act = exec k
        in F.viaModel l act
