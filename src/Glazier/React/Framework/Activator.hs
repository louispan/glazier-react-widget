{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Activator
    ( Activator(..)
    , RefActivator
    , suppressActivatorExecutor
    , contramapActivatorExecutor
    , triggersRefActivator
    , triggersRefActivator'
    -- , displayRefActivator
    -- , displayRefActivator'
    -- , addHandler
    , ActivatorModeller(..)
    ) where

import Control.DeepSeq
import Control.Lens
-- import Control.Monad
import Data.Diverse.Lens
import qualified Data.DList as DL
-- import Data.Foldable
import Data.IORef
import Data.Semigroup
import Data.Generics.Product
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
-- import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
-- import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Parameterized.Data.Monoid as P

------------------------------------------

newtype Activator m r x = Activator
    { runActivator :: F.Executor x () -- effectful interpreters
                   -> r -- Handler env
                   -> m () -- return the monadic action to commit the activation
    }

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type RefActivator m v s x = Activator m (IORef v, ReifiedLens' v s) x

instance Monad m => Semigroup (Activator m r a) where
    (Activator f) <> (Activator g) = Activator $ \exec env ->
        f exec env >>
        g exec env

instance Monad m => Monoid (Activator m r x) where
    mempty = Activator $ \_ _ -> pure ()
    mappend = (<>)

-- instance R.MonadReactor m => F.IORefModel (Activator m s s a) (Activator m v (IORef s) a) where
--     ioRefModel (Activator g) = Activator $ \ref (Lens this) exec -> do
--         obj <- R.doReadIORef ref
--         let ref' = obj ^. this
--         g ref' (Lens id) exec

------------------------------------------

type instance P.PNullary (Activator m r) x = Activator m r x

instance Applicative m => P.PMEmpty (Activator m r) (Which '[]) where
    pmempty = Activator $ \_ _ -> pure ()

instance ( Monad m
         , Reinterpret' x3 x1
         , Reinterpret' x3 x2
         , x3 ~ AppendUnique x1 x2
         ) => P.PSemigroup (Activator m r) (Which x1) (Which x2) (Which x3) where
    (Activator f) `pmappend` (Activator g) = Activator $ \exec env ->
        f (F.suppressExecutor reinterpret' exec) env >>
        g (F.suppressExecutor reinterpret' exec) env

------------------------------------------

-- | Ignore certain commands contravariantly
suppressActivatorExecutor :: (x -> Maybe x') ->  Activator m r x -> Activator m r x'
suppressActivatorExecutor f (Activator act) = Activator $ \exec env -> act (F.suppressExecutor f exec) env

-- | Map a function to the commands contravariantly
contramapActivatorExecutor :: (x -> x') -> Activator m r x -> Activator m r x'
contramapActivatorExecutor f (Activator act) = Activator $ \exec env -> act (F.contramapExecutor f exec) env

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
triggersRefActivator
    :: (R.MonadReactor m, NFData x)
    => [F.Trigger x]
    -> RefActivator m v (DL.DList R.Listener) x
triggersRefActivator triggers = Activator $ \(F.Executor exec) (ref, Lens this) -> do
    cbs <- traverse (traverse {- tuple traverse -} (\t' ->
               R.mkCallback t' exec) . F.runTrigger) triggers
    R.doModifyIORef' ref (this %~ (`DL.append` DL.fromList cbs))

-- | Variation of 'triggersRefActivator' using a state of @(F.ComponentModel, Many s)@
triggersRefActivator'
    :: (R.MonadReactor m, NFData x, HasType (DL.DList R.Listener) s)
    => [F.Trigger x]
    -> RefActivator m v (F.ComponentModel, s) x
triggersRefActivator' = F.viaModel (_2.typed) . triggersRefActivator

-- -- | Store the rendering instructions inside a render callback and add it to this state's render holder.
-- displayRefActivator
--     :: (R.MonadReactor m)
--     => F.Display m (IORef v) ()
--     -> RefActivator m v R.Renderer (Which '[])
-- displayRefActivator (F.Display disp) = Activator $ \(ref, Lens this) _ -> do
--     rnd <- R.mkRenderer (disp ref)
--     R.doModifyIORef' ref (this .~ rnd)

-- -- | Variation of 'displayRefActivator' using a state of @(F.ComponentModel, Many s)@
-- displayRefActivator'
--     :: (R.MonadReactor m, UniqueMember R.Renderer s)
--     => F.Display m (IORef v) ()
--     -> RefActivator m v (F.ComponentModel, Many s) (Which '[])
-- displayRefActivator' = F.viaModel (_2.item) . displayRefActivator

-- -- | Internal function: Converts a handler to a form that can be chained inside an Activator
-- mkIOHandler
--     :: R.MonadReactor m
--     => F.Handler m r x a b
--     -> F.Executor x ()
--     -> r
--     -> m (DL.DList a -> IO (DL.DList b))
-- mkIOHandler (F.Handler hdl) exec env = R.mkIO go
--   where
--     go cs = fold <$> traverse (hdl exec env) (DL.toList cs)

-- -- | Add a handler so that it is piped before the input 'Executor' in an 'Activator'
-- addHandler :: R.MonadReactor m => F.Handler m r x a b -> Activator m r a -> Activator m r b
-- addHandler f (Activator g) = Activator $ \(F.Executor exec) env -> do
--     f' <- mkIOHandler f env
--     g (F.Executor $ f' >=> exec) env

------------------------------------------

newtype ActivatorModeller m v x s = ActivatorModeller { runActivatorModeller :: RefActivator m v s x }

type instance F.Modeller (ActivatorModeller m v x) s = RefActivator m v s x

instance F.ViaModel (ActivatorModeller m v x) where
    viaModel l (Activator f) = Activator $ \exec (ref, Lens this) ->
        f exec (ref, Lens (this.l))
