{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Framework.Activator where

import Control.DeepSeq
import Control.Lens
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Widget as F
import qualified GHCJS.Types as J
import qualified GHCJS.Foreign.Callback as J

newtype Activator m a c v s = Activator
    { runActivator :: IORef v -- IORef that contains the parent state
                   -> ReifiedLens' v s -- how to get to the child state
                   -> F.Handler m v s a c -- externally provided handlers
                   -> F.Executor (DL.DList c) -- effectful interpreters
                   -- return the monadic action to commit the activation
                   -> m ()
    }

-- | identity for 'andActivator'
inert :: Monad m => Activator m (Which '[]) c v s
inert = Activator $ \_ _ _ _ -> pure ()

-- | It is okay to combine activators the expect the same @a@ action, hence the use of 'AppendUnique'
andActivator
    :: ( Monad m
       , Reinterpret' (AppendUnique a1 a2) a1
       , Reinterpret' (AppendUnique a1 a2) a2
       )
    => Activator m (Which a1) c v s
    -> Activator m (Which a2) c v s
    -> Activator m (Which (AppendUnique a1 a2)) c v s
andActivator (Activator f) (Activator g) = Activator $ \ref this hdl exec ->
    f ref this (F.lfilterHandler reinterpret' hdl) exec >>
    g ref this (F.lfilterHandler reinterpret' hdl) exec

-- | Create callbacks from triggers and add it to this state's dlist of listeners.
activateTriggers
    :: (R.MonadReactor m, NFData a, UniqueMember (DL.DList R.Listener) s)
    => [(J.JSString, J.JSVal -> IO a)]
    -> Activator m a c v (Many s)
activateTriggers triggers = Activator $ \ref (Lens this) (F.Handler hdl) exec -> do
    cbs <- traverse (traverse (\t -> R.mkCallback t (hdl ref (Lens this)) exec)) triggers
    R.doModifyIORef' ref ((this.item) %~ (`DL.append` DL.fromList cbs))

-- | Store the rendering instructions inside a render callback and add it to this state's render holder.
activateDisplay
    :: (R.MonadReactor m, UniqueMember (J.Callback (IO J.JSVal)) s)
    => F.Display m (IORef v)
    -> IORef v
    -> Lens' v (Many s)
    -> m ()
activateDisplay (F.Display disp) ref this = do
    rnd <- R.mkRenderer (disp ref)
    R.doModifyIORef' ref ((this.item) .~ rnd)

-- FIXME: What about Modeller?

-- viaModelActivator :: Monad m => Lens' t s -> Activator m a c v s -> Activator m a c v t
-- viaModelActivator l (Activator f) =
--     Activator $ \ref (Lens this) hdl exec ->
--         -- FIXME: l is wrong in (F.viaModel' l hdl)
--         -- Wrong direction! How do I convert Handler t to Handler s?
--         f ref (Lens (this.l)) (F.viaModel' l hdl) exec
