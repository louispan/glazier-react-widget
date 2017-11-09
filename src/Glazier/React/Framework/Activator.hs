{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Activator
    ( Activator(..)
    , Activator'
    , triggersActivator
    , displayActivator
    , addHandler
    , ActivatorModeller(..)
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Parameterized.Data.Monoid as P

------------------------------------------

newtype Activator m r a = Activator
    { runActivator :: r -- Handler env
                   -> F.Executor a () -- effectful interpreters
                   -> m () -- return the monadic action to commit the activation
    }

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type Activator' m v s a = Activator m (IORef v, ReifiedLens' v s) a

instance Monad m => Semigroup (Activator m r a) where
    (Activator f) <> (Activator g) = Activator $ \env exec ->
        f env exec >>
        g env exec

instance Monad m => Monoid (Activator m r a) where
    mempty = Activator $ \_ _ -> pure ()
    mappend = (<>)

-- instance R.MonadReactor m => F.IORefModel (Activator m s s a) (Activator m v (IORef s) a) where
--     ioRefModel (Activator g) = Activator $ \ref (Lens this) exec -> do
--         obj <- R.doReadIORef ref
--         let ref' = obj ^. this
--         g ref' (Lens id) exec

------------------------------------------

type instance P.PNullary (Activator m r) a = Activator m r a

instance Applicative m => P.PMEmpty (Activator m r) (Which '[]) where
    pmempty = Activator $ \_ _ -> pure ()

instance ( Monad m
         , Reinterpret' c3 c1
         , Reinterpret' c3 c2
         , c3 ~ AppendUnique c1 c2
         ) => P.PSemigroup (Activator m r) (Which c1) (Which c2) (Which c3) where
    (Activator f) `pmappend` (Activator g) = Activator $ \env exec ->
        f env (F.suppressExecutor reinterpret' exec) >>
        g env (F.suppressExecutor reinterpret' exec)

------------------------------------------

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
triggersActivator
    :: (R.MonadReactor m, NFData a, UniqueMember (DL.DList R.Listener) s)
    => [F.Trigger a]
    -> Activator' m v (Many s) a
triggersActivator triggers = Activator $ \(ref, Lens this) (F.Executor exec) -> do
    cbs <- traverse (traverse {- tuple traverse -} (\t' -> R.mkCallback t' exec) . F.runTrigger) triggers
    R.doModifyIORef' ref ((this.item) %~ (`DL.append` DL.fromList cbs))

-- | Store the rendering instructions inside a render callback and add it to this state's render holder.
displayActivator
    :: (R.MonadReactor m, UniqueMember R.Renderer s)
    => F.Display m (IORef v) ()
    -> Activator' m v (Many s) (Which '[])
displayActivator (F.Display disp) = Activator $ \(ref, Lens this) _ -> do
    rnd <- R.mkRenderer (disp ref)
    R.doModifyIORef' ref ((this.item) .~ rnd)

-- | Internal function: Converts a handler to a form that can be chained inside an Activator
mkIOHandler
    :: R.MonadReactor m
    => F.Handler m r a b
    -> r
    -> m (DL.DList a -> IO (DL.DList b))
mkIOHandler (F.Handler hdl) env = R.mkIO go
  where
    go cs = fold <$> traverse (hdl env) (DL.toList cs)

-- | Add a handler so that it is piped before the input 'Executor' in an 'Activator'
addHandler :: R.MonadReactor m => F.Handler m r a b -> Activator m r a -> Activator m r b
addHandler f (Activator g) = Activator $ \env (F.Executor exec) -> do
    f' <- mkIOHandler f env
    g env (F.Executor $ f' >=> exec)

------------------------------------------

newtype ActivatorModeller m v a s = ActivatorModeller { runActivatorModeller :: Activator' m v s a }

type instance F.Modeller (ActivatorModeller m v a) s = Activator' m v s a

instance F.ViaModel (ActivatorModeller m v a) where
    viaModel l (Activator f) = Activator $ \(ref, Lens this) exec ->
        f (ref, Lens (this.l)) exec
