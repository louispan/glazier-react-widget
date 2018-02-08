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
import qualified GHCJS.Types as J
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

-- -- | Type restricted version of 'pure' where the executor environment @c@ is set to @Which '[]@
-- execute :: a -> Executor m x (Which '[]) a
-- execute = pure

-- | Lift a handler into an Executor where the executor environment @c@ is set to the output of then handler @b@
-- Although, the executor environment is not used, this is handy for creating a handler to be used in `controls'`
delegate :: F.Handler m r a b -> ExHandler m r x b a b
delegate = pure
------------------------------------------------------

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
-- Using @AllowAmbiguousTypes@ instead of @Proxy@
triggers :: forall t m v s x c.
    ( R.MonadReactor x m
    , NFData c
    , HasItemTag' t [R.Listener] s
    )
    => [F.Trigger c]
    -> ProtoActivator m v s x c
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

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
-- Using @AllowAmbiguousTypes@ instead of @Proxy@
trigger :: forall t m v s x c.
    ( R.MonadReactor x m
    , NFData c
    , HasItemTag' t [R.Listener] s
    )
    => J.JSString
    -> (J.JSVal -> IO (DL.DList c))
    -> ProtoActivator m v s x c
trigger n f = Executor $ \k -> F.Activator $ act k
  where
    act k (F.Object ref (Lens this)) = do
        (ds, cb) <- R.mkCallback f k
        R.doModifyIORef' ref $ \obj ->
            obj & this._2.itemTag' @t %~ ((n, cb) :)
                & this._1.field @"disposeOnRemoved" %~ (<> ds)

-- | Use the given handler to transform the Executor's environment
-- Simple version where the handler input type must match the Executor environment type.
handleBeforeExecuting' ::
    (Monad m)
    => r
    -> F.Handler m r a b
    -> Executor m x a y
    -> Executor m x b y
handleBeforeExecuting' env (F.Handler hdl) (Executor exec) = Executor $ \k ->
    let k' cs = (fold <$> traverse (hdl env) (DL.toList cs)) >>= k
    in exec k'

-- | Use the given handler to transform the Executor's environment
-- Complex version where the handler input type @Which a@ doesn't have to match the Executor environment type @Which c@,
-- but instead just need to be a 'Injected' subset.
handleBeforeExecuting :: forall a b c d m x r y.
    ( Monad m
    , Injected a c b d
    )
    => r
    -> F.Handler m r (Which a) (Which b)
    -> Executor m x (Which c) y
    -> Executor m x (Which d) y
handleBeforeExecuting env hdl exec =
        -- hdl' :: F.Handler m r (Which c) (Which d)
    let hdl' = injected @_ @_ @c hdl
    in handleBeforeExecuting' env hdl' exec

-- | Use left executor's handler to tranform the right Executor's environment.
-- Simple version where:
-- 1. the handler executor environment type must match the handler output type;
-- 2. and where the handler input type must match the right excutor's environment type.
controls' ::
    ( Monad m
    , F.IsReader r y
    )
    => ExHandler m r x b a b -- handler
    -> Executor m x a y
    -> Executor m x b y
controls' (Executor exec1) exec2 = Executor $ \k ->
    let hdl1 = exec1 k
    in F.fromReader $ \env ->
        let Executor exec2' = handleBeforeExecuting' env hdl1 exec2
            y = exec2' k
        in (F.toReader y) env

-- | Use left executor's handler to tranform the right Executor's environment.
-- Complex version where:
-- 1. the handler executor environment type @Which c1@ doesn't have to match the handler output type @Which b@,
-- but instead handler executor environment type is added to the resultant executor environment type @Which c4@;
-- 2. the handler input type  @Which a@ doesn't have to match the right excutor's environment type @Which c2@
-- but instead just need to be a 'Injected' subset.
-- Ie. replace the @a@ in @c2@ with @b@ (this is @c3@), but now we also need @c1@ in the environment @c4@
-- so @c4@ = @c1@ + @c3@.
controls :: forall m r x c1 c2 c3 c4 a b y.
    ( Monad m
    , c4 ~ AppendUnique c1 c3
    , Injected a c2 b c3
    , Diversify c1 c4
    , Diversify c3 c4
    , F.IsReader r y
    )
    => ExHandler m r x (Which c1) (Which a) (Which b) -- handler
    -> Executor m x (Which c2) y
    -> Executor m x (Which c4) y
controls exec1 exec2 =
    let exec1' :: Executor m x (Which c4) (F.Handler m r (Which a) (Which b))
        exec1' = withExecutor diversify exec1
        exec1'' :: Executor m x (Which c4) (F.Handler m r (Which c2) (Which c3))
        exec1'' = injected @_ @_ @c2 <$> exec1'
        exec1''' :: Executor m x (Which c4) (F.Handler m r (Which c2) (Which c4))
        exec1''' = rmap diversify <$> exec1''
    in controls' exec1''' exec2

-- | Convenience function to create an activator
-- given triggers and a handler.
-- Simple version using 'controls''
controlledTriggers' :: forall t m v s x a b.
    ( R.MonadReactor x m
    , NFData a
    , HasItemTag' t [R.Listener] s
    )
    => [F.Trigger a]
    -> ProtoHandler m v s x b a b
    -> ProtoActivator m v s x b
controlledTriggers' ts hdl = hdl `controls'` triggers @t ts

-- | Convenience function to create an activator
-- given triggers and a handler.
-- Complex version using 'controls'
controlledTriggers :: forall t m v s x c1 c2 c3 c4 a b.
    ( R.MonadReactor x m
    , NFData (Which c2)
    , HasItemTag' t [R.Listener] s
    , c4 ~ AppendUnique c1 c3
    , Injected a c2 b c3
    , Diversify c1 c4
    , Diversify c3 c4
    )
    => [F.Trigger (Which c2)]
    -> ProtoHandler m v s x (Which c1) (Which a) (Which b)
    -> ProtoActivator m v s x (Which c4)
controlledTriggers ts hdl = hdl `controls` triggers @t ts

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
type ProtoHandler m v s x z a b = Executor m x z (F.ObjHandler m v (F.ComponentPlan x m, s) a b)

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
type ProtoActivator m v s x y = Executor m x y (F.ObjActivator m v (F.ComponentPlan x m, s))

newtype ExObjActivatorOnModel m v x y s = ExObjActivatorOnModel {
    runExObjActivatorOnModel :: ExObjActivator m v s x y
    }

type instance F.OnModel (ExObjActivatorOnModel m v x y) s = ExObjActivator m v s x y

instance F.ViaModel (ExObjActivatorOnModel m v x y) where
    viaModel l (Executor exec) = Executor $ \k ->
        let act = exec k
        in F.viaModel l act
