{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Framework.Activator where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
-- import qualified Glazier.React.Framework.Widget as F

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
activateTriggers
    :: (R.MonadReactor m, NFData c, UniqueMember (DL.DList R.Listener) s)
    => [(J.JSString, J.JSVal -> IO (DL.DList c))]
    -> Activator m c v (Many s)
activateTriggers triggers = Activator $ \ref (Lens this) exec -> do
    cbs <- traverse (traverse (\t -> R.mkCallback t exec)) triggers
    R.doModifyIORef' ref ((this.item) %~ (`DL.append` DL.fromList cbs))

-- | Store the rendering instructions inside a render callback and add it to this state's render holder.
activateDisplay
    :: (R.MonadReactor m, UniqueMember (J.Callback (IO J.JSVal)) s)
    => F.Display m (IORef v)
    -> IORef v
    -> Lens' v (Many s)
    -- -> F.Executor (DL.DList (Which '[]))
    -> m ()
activateDisplay (F.Display disp) ref this = do
    rnd <- R.mkRenderer (disp ref)
    R.doModifyIORef' ref ((this.item) .~ rnd)

-- FIXME: What about Modeller?

viaModelActivator :: Monad m => Lens' t s -> Activator m c v s -> Activator m c v t
viaModelActivator l (Activator f) =
    Activator $ \ref (Lens this) exec ->
        -- FIXME: l is wrong in (F.viaModel' l hdl)
        -- Wrong direction! How do I convert Handler t to Handler s?
        f ref (Lens (this.l)) exec


-- -- | FIXME: Internal function, do not expose
-- -- Prefix a given execution to an Activator
-- preExecute :: (DL.DList c -> IO (DL.DList d)) -> Activator m c v s -> Activator m d v s
-- preExecute f (Activator g) = Activator $ \ref this exec -> g ref this (f >=> exec)

-- | FIXME: Internal function, do not expose
-- Converts a handler to a form that can be chained inside an Activator
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
