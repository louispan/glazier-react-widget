{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Framework.Activator
    ( Activator(..)
    , triggersActivator
    , displayActivator
    , addHandler
    ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Widget as F

newtype Activator m c v s = Activator
    { runActivator :: IORef v -- IORef that contains the parent state
                   -> ReifiedLens' v s -- how to get to the child state
                   -> F.Executor (DL.DList c) -- effectful interpreters
                   -- return the monadic action to commit the activation
                   -> m ()
    }

instance Monad m => Semigroup (Activator m c v s) where
    (Activator f) <> (Activator g) = Activator $ \ref this exec ->
        f ref this exec >>
        g ref this exec

instance Monad m => Monoid (Activator m c v s) where
    mempty = Activator $ \_ _ _ -> pure ()
    mappend = (<>)


-- | Create callbacks from triggers and add it to this state's dlist of listeners.
triggersActivator
    :: (R.MonadReactor m, NFData c, UniqueMember (DL.DList R.Listener) s)
    => [(J.JSString, J.JSVal -> IO (DL.DList c))]
    -> Activator m c v (Many s)
triggersActivator triggers = Activator $ \ref (Lens this) exec -> do
    cbs <- traverse (traverse (\t -> R.mkCallback t exec)) triggers
    R.doModifyIORef' ref ((this.item) %~ (`DL.append` DL.fromList cbs))

-- | Store the rendering instructions inside a render callback and add it to this state's render holder.
displayActivator
    :: (R.MonadReactor m, UniqueMember R.Renderer s)
    => F.Display m (IORef v)
    -> Activator m (Which '[]) v (Many s)
displayActivator (F.Display disp) = Activator $ \ref (Lens this) _ -> do
    rnd <- R.mkRenderer (disp ref)
    R.doModifyIORef' ref ((this.item) .~ rnd)

instance Monad m => F.ViaModel (Activator m c v) where
    viaModel l (Activator f) = Activator $ \ref (Lens this) exec ->
        f ref (Lens (this.l)) exec

instance F.Modeller (Activator m c v s) (Activator m c v) s where
    toModeller = id
    fromModeller = id

instance R.MonadReactor m => F.IORefModel (Activator m c s s) (Activator m c v (IORef s)) where
    ioRefModel (Activator g) = Activator $ \ref (Lens this) exec -> do
        obj <- R.doReadIORef ref
        let ref' = obj ^. this
        g ref' (Lens id) exec

-- | Internal function: Converts a handler to a form that can be chained inside an Activator
mkIOHandler
    :: R.MonadReactor m
    => F.Handler m v s c d
    -> IORef v
    -> ReifiedLens' v s
    -> m (DL.DList c -> IO (DL.DList d))
mkIOHandler (F.Handler hdl) ref this = R.mkIO go
  where
    go cs = fold <$> traverse (hdl ref this) (DL.toList cs)

-- | Add a handler so that it is piped before the input 'Executor' in an 'Activator'
addHandler :: R.MonadReactor m => F.Handler m v s c d -> Activator m c v s -> Activator m d v s
addHandler f (Activator g) = Activator $ \ref this exec -> do
    f' <- mkIOHandler f ref this
    g ref this (f' >=> exec)
