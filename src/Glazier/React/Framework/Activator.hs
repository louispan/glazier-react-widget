{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Activator
    ( Activator(..)
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
-- import qualified Glazier.React.Framework.Parameterized as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Parameterized.Data.Monoid as P

newtype Activator m v s a = Activator
    { runActivator :: IORef v -- IORef that contains the parent state
                   -> ReifiedLens' v s -- how to get to the child state
                   -> F.Executor a () -- effectful interpreters
                   -- return the monadic action to commit the activation
                   -> m ()
    }

instance Monad m => Semigroup (Activator m v s a) where
    (Activator f) <> (Activator g) = Activator $ \ref this exec ->
        f ref this exec >>
        g ref this exec

instance Monad m => Monoid (Activator m v s a) where
    mempty = Activator $ \_ _ _ -> pure ()
    mappend = (<>)

instance R.MonadReactor m => F.IORefModel (Activator m s s a) (Activator m v (IORef s) a) where
    ioRefModel (Activator g) = Activator $ \ref (Lens this) exec -> do
        obj <- R.doReadIORef ref
        let ref' = obj ^. this
        g ref' (Lens id) exec

------------------------------------------

-- instance F.IsPNullary (Activator m v s a) (Activator m v s) a where
--     toPNullary = id
--     fromPNullary = id

instance Applicative m => P.PMEmpty (Activator m v s) (Which '[]) where
    pmempty = Activator $ \_ _ _ -> pure ()

instance ( Monad m
         , Reinterpret' c3 c1
         , Reinterpret' c3 c2
         , c3 ~ AppendUnique c1 c2
         ) => P.PSemigroup (Activator m v s) (Which c1) (Which c2) (Which c3) where
    (Activator f) `pmappend` (Activator g) = Activator $ \ref this exec ->
        f ref this (F.suppressExecutor reinterpret' exec) >>
        g ref this (F.suppressExecutor reinterpret' exec)

------------------------------------------

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
triggersActivator
    :: (R.MonadReactor m, NFData a, UniqueMember (DL.DList R.Listener) s)
    => [F.Trigger a]
    -> Activator m v (Many s) a
triggersActivator triggers = Activator $ \ref (Lens this) (F.Executor exec) -> do
    cbs <- traverse (traverse (\t' -> R.mkCallback t' exec) . F.runTrigger) triggers
    R.doModifyIORef' ref ((this.item) %~ (`DL.append` DL.fromList cbs))

-- | Store the rendering instructions inside a render callback and add it to this state's render holder.
displayActivator
    :: (R.MonadReactor m, UniqueMember R.Renderer s)
    => F.Display m (IORef v) ()
    -> Activator m v (Many s) (Which '[])
displayActivator (F.Display disp) = Activator $ \ref (Lens this) _ -> do
    rnd <- R.mkRenderer (disp ref)
    R.doModifyIORef' ref ((this.item) .~ rnd)

-- | Internal function: Converts a handler to a form that can be chained inside an Activator
mkIOHandler
    :: R.MonadReactor m
    => F.Handler m v s a b
    -> IORef v
    -> ReifiedLens' v s
    -> m (DL.DList a -> IO (DL.DList b))
mkIOHandler (F.Handler hdl) ref this = R.mkIO go
  where
    go cs = fold <$> traverse (hdl ref this) (DL.toList cs)

-- | Add a handler so that it is piped before the input 'Executor' in an 'Activator'
addHandler :: R.MonadReactor m => F.Handler m v s a b -> Activator m v s a -> Activator m v s b
addHandler f (Activator g) = Activator $ \ref this (F.Executor exec) -> do
    f' <- mkIOHandler f ref this
    g ref this (F.Executor $ f' >=> exec)

------------------------------------------

newtype ActivatorModeller m v a s = ActivatorModeller { runActivatorModeller :: Activator m v s a }

instance F.IsModeller (Activator m v s a) (ActivatorModeller m v a) s where
    toModeller = ActivatorModeller
    fromModeller = runActivatorModeller

instance Monad m => F.ViaModel (ActivatorModeller m v a) where
    viaModel l (ActivatorModeller (Activator f)) = ActivatorModeller . Activator $ \ref (Lens this) exec ->
        f ref (Lens (this.l)) exec
