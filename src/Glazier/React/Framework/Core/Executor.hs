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

module Glazier.React.Framework.Core.Executor where

import Control.DeepSeq
import Control.Lens
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.Generics.Product
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Activator as F
import qualified Glazier.React.Framework.Core.Handler as F
import qualified Glazier.React.Framework.Core.IsReader as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

-- | A reader with specialized executing environment
newtype Executor m c a = Executor { runExecutor :: (DL.DList c -> m ()) -> a }
    deriving Functor

instance F.IsReader (DL.DList c -> m ()) (Executor m c a) where
    type ReaderResult (DL.DList c -> m ()) (Executor m c a) = a
    fromReader = Executor
    toReader = runExecutor

instance Applicative (Executor m c) where
    pure r = Executor $ const r
    (Executor exec1) <*> (Executor exec2) = Executor $ \k -> exec1 k (exec2 k)

instance Monad (Executor m c) where
    (Executor exec) >>= f = Executor $ \k -> runExecutor (f (exec k)) k

suppressExecutor :: (c -> Maybe c') -> Executor m c a -> Executor m c' a
suppressExecutor f (Executor exec) = Executor $ \k -> exec (k . foldMap go)
  where
    go c = maybe mempty DL.singleton (f c)

withExecutor :: (c -> c') -> Executor m c a -> Executor m c' a
withExecutor f (Executor exec) = Executor $ \k -> exec (k . fmap f)

-- -- | Type restricted version of 'pure' where the executor environment @c@ is set to @Which '[]@
-- execute :: a -> Executor m x (Which '[]) a
-- execute = pure

-- | Lift a handler into an Executor where the executor environment @c@ is set to the output of then handler @b@
-- Although, the executor environment is not used, this is handy for creating a handler to be used in `controls'`
delegate :: F.Handler m r a b -> ExHandler m r b a b
delegate = pure
------------------------------------------------------

-- | Create callbacks and add it to this state's dlist of listeners.
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
trigger :: forall t m v s c.
    ( R.MonadReactor m
    , NFData c
    , HasItemTag' t [R.Listener] s
    )
    => J.JSString
    -> (J.JSVal -> IO (DL.DList c))
    -> ProtoActivator m v s c
trigger n f = Executor $ \k -> F.Activator $ act k
  where
    act k (F.Obj ref its) = do
        (ds, cb) <- R.doMkCallback f k
        R.doModifyIORef' ref $ \obj ->
            obj & its.F.model.itemTag' @t %~ ((n, cb) :)
                & its.F.plan.field @"disposeOnRemoved" %~ (<> ds)

-- | Use the given handler to transform the Executor's environment
-- Simple version where the handler input type must match the Executor environment type.
handleBeforeExecuting' ::
    (Monad m)
    => r
    -> F.Handler m r a b
    -> Executor m a y
    -> Executor m b y
handleBeforeExecuting' env (F.Handler hdl) (Executor exec) = Executor $ \k ->
    let k' cs = (fold <$> traverse (hdl env) (DL.toList cs)) >>= k
    in exec k'

-- | Use the given handler to transform the Executor's environment
-- Complex version where the handler input type @Which a@ doesn't have to match the Executor environment type @Which c@,
-- but instead just need to be a 'Injected' subset.
handleBeforeExecuting :: forall a b c d m r y.
    ( Monad m
    , Injected a c b d
    )
    => r
    -> F.Handler m r (Which a) (Which b)
    -> Executor m (Which c) y
    -> Executor m (Which d) y
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
    => ExHandler m r b a b -- handler
    -> Executor m a y
    -> Executor m b y
controls' (Executor exec1) exec2 = Executor $ \k ->
    let hdl1 = exec1 k
    in F.fromReader $ \env ->
        let Executor exec2' = handleBeforeExecuting' env hdl1 exec2
            y = exec2' k
        in (F.toReader y) env
infixl 4 `controls'` -- like fmap

-- | Use left executor's handler to tranform the right Executor's environment.
-- Complex version where:
-- 1. the handler executor environment type @Which c1@ doesn't have to match the handler output type @Which b@,
-- but instead handler executor environment type is added to the resultant executor environment type @Which c4@;
-- 2. the handler input type  @Which a@ doesn't have to match the right excutor's environment type @Which c2@
-- but instead just need to be a 'Injected' subset.
-- Ie. replace the @a@ in @c2@ with @b@ (this is @c3@), but now we also need @c1@ in the environment @c4@
-- so @c4@ = @c1@ + @c3@.
controls :: forall m r c1 c2 c3 c4 a b y.
    ( Monad m
    , c4 ~ AppendUnique c1 c3
    , Injected a c2 b c3
    , Diversify c1 c4
    , Diversify c3 c4
    , F.IsReader r y
    )
    => ExHandler m r (Which c1) (Which a) (Which b) -- handler
    -> Executor m (Which c2) y
    -> Executor m (Which c4) y
controls exec1 exec2 =
    let exec1' :: ExHandler m r (Which c4) (Which a) (Which b)
        exec1' = withExecutor diversify exec1
        exec1'' :: ExHandler m r (Which c4) (Which c2) (Which c3)
        exec1'' = injected @_ @_ @c2 <$> exec1'
        exec1''' :: ExHandler m r (Which c4) (Which c2) (Which c4)
        exec1''' = rmap diversify <$> exec1''
    in controls' exec1''' exec2
infixl 4 `controls` -- like fmap

-- | Convenience function to create an activator
-- given triggers and a handler.
-- Simple version using 'controls''
controlledTrigger' :: forall t m v s a b.
    ( R.MonadReactor m
    , NFData a
    , HasItemTag' t [R.Listener] s
    )
    => J.JSString
    -> (J.JSVal -> IO (DL.DList a))
    -> ProtoHandler m v s b a b
    -> ProtoActivator m v s b
controlledTrigger' n f hdl = hdl `controls'` trigger @t n f

-- | Convenience function to create an activator
-- given triggers and a handler.
-- Complex version using 'controls'
controlledTrigger :: forall t m v s c1 c2 c3 c4 a b.
    ( R.MonadReactor m
    , NFData (Which c2)
    , HasItemTag' t [R.Listener] s
    , c4 ~ AppendUnique c1 c3
    , Injected a c2 b c3
    , Diversify c1 c4
    , Diversify c3 c4
    )
    => J.JSString
    -> (J.JSVal -> IO (DL.DList (Which c2)))
    -> ProtoHandler m v s (Which c1) (Which a) (Which b)
    -> ProtoActivator m v s (Which c4)
controlledTrigger n f hdl = hdl `controls` trigger @t n f

------------------------------------------------------

newtype PExHandler m r zab = PExHandler
    { runPExHandler :: Executor m (P.At0 zab) (F.Handler m r (P.At1 zab) (P.At2 zab))
    }

type instance P.PNullary (PExHandler m r) (z, a, b) = Executor m z (F.Handler m r a b)

instance Applicative m => P.PMEmpty (PExHandler m r) (Which '[], Which '[], Which '[]) where
    pmempty = Executor $ const P.pmempty

-- | A friendlier constraint synonym for 'Executor' 'pmappend'.
type PmappendExecutor z1 z2 z3 =
    ( Diversify z1 z3
    , Diversify z2 z3
    , z3 ~ AppendUnique z1 z2
    )

-- | type restricted version of 'P.pmempty' for 'ExHandler'
nilExHandler :: Applicative m => ExHandler m r (Which '[]) (Which '[]) (Which '[])
nilExHandler = P.pmempty

-- | Undecidableinstances!
instance ( Monad m
         , PmappendExecutor z1 z2 z3
         , ChooseBetween a1 a2 a3 b1 b2 b3) =>
         P.PSemigroup (PExHandler m r)
              (Which z1, Which a1, Which b1)
              (Which z2, Which a2, Which b2)
              (Which z3, Which a3, Which b3) where
    x `pmappend` y = Executor $ \k ->
        let hdl1 = runExecutor (withExecutor diversify x) k
            hdl2 = runExecutor (withExecutor diversify y) k
        in (hdl1 +||+ hdl2)

-- | type restricted version of 'P.pmappend' for 'ExHandler'
andExHandler ::
    ( Monad m
    , PmappendExecutor z1 z2 z3
    , ChooseBetween a1 a2 a3 b1 b2 b3
    )
    => ExHandler m r (Which z1) (Which a1) (Which b1)
    -> ExHandler m r (Which z2) (Which a2) (Which b2)
    -> ExHandler m r (Which z3) (Which a3) (Which b3)
andExHandler = P.pmappend
infixr 6 `andExHandler` -- like mappend

------------------------------------------------------

type ExHandler m r z a b = Executor m z (F.Handler m r a b)
type ExObjHandler m v s z a b = Executor m z (F.ObjHandler m v s a b)
type ProtoHandler m v s z a b = Executor m z (F.SceneHandler m v s a b)

newtype ExObjHandlerOnSpec m v z a b s = ExObjHandlerOnSpec {
    runExObjHandlerOnSpec :: ExObjHandler m v s z a b
    }

type instance F.OnSpec (ExObjHandlerOnSpec m v z a b) s = ExObjHandler m v s z a b

instance F.ViaSpec (ExObjHandlerOnSpec m v z a b) where
    viaSpec l (Executor exec) = Executor $ \k ->
        let hdl = exec k
        in F.viaSpec l hdl


------------------------------------------------------

newtype PExActivator m r y = PExActivator
    { runPExActivator :: Executor m y (F.Activator m r)
    }

type instance P.PNullary (PExActivator m r) y = Executor m y (F.Activator m r)

instance Monad m => P.PMEmpty (PExActivator m r) (Which '[]) where
    pmempty = Executor $ const mempty

-- | type restricted version of 'P.pmempty' for 'ExActivator'
nilExActivator :: Monad m => ExActivator m r (Which '[])
nilExActivator = P.pmempty

-- | Undecidableinstances!
instance ( Monad m
         , PmappendExecutor y1 y2 y3) =>
         P.PSemigroup (PExActivator m r)
              (Which y1)
              (Which y2)
              (Which y3) where
    x `pmappend` y = Executor $ \k ->
        let act1 = runExecutor (withExecutor diversify x) k
            act2 = runExecutor (withExecutor diversify y) k
        in (act1 <> act2)

-- | type restricted version of 'P.pmappend' for 'ExActivator'
andExActivator ::
    ( Monad m
    , PmappendExecutor y1 y2 y3
    )
    => ExActivator m r (Which y1)
    -> ExActivator m r (Which y2)
    -> ExActivator m r (Which y3)
andExActivator = P.pmappend
infixr 6 `andExActivator` -- like mappend
------------------------------------------------------

type ExActivator m r y = Executor m y (F.Activator m r)
type ExObjActivator m v s y = Executor m y (F.ObjActivator m v s)
type ProtoActivator m v s y = Executor m y (F.SceneActivator m v s)

newtype ExObjActivatorOnSpec m v y s = ExObjActivatorOnSpec {
    runExObjActivatorOnSpec :: ExObjActivator m v s y
    }

type instance F.OnSpec (ExObjActivatorOnSpec m v y) s = ExObjActivator m v s y

instance F.ViaSpec (ExObjActivatorOnSpec m v y) where
    viaSpec l (Executor exec) = Executor $ \k ->
        let act = exec k
        in F.viaSpec l act