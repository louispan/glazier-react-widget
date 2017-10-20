{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Framework.Activator where

import Data.Diverse
import qualified Data.DList as DL
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified GHCJS.Types as J

type Activator m s' s a c =
    IORef s -- IORef that contains the parent state
    -> (s -> m s') -- how sto get to the child state
    -> F.Handler m s' a c -- externally provided handlers
    -> F.Executor (DL.DList c) -- effectful interpreters
    -- return the monadic action to commit the activation
    -> m ()

-- | identity for 'andActivator'
inert :: Monad m => Activator m s' s (Which '[]) c
inert _ _ _ _ = pure ()

-- | It is okay to combine activators the expect the same @a@ action, hence the use of 'AppendUnique'
andActivator
    :: ( Monad m
       , Reinterpret' (AppendUnique a1 a2) a1
       , Reinterpret' (AppendUnique a1 a2) a2
       )
    => Activator m s s' (Which a1) c
    -> Activator m s s' (Which a2) c
    -> Activator m s s' (Which (AppendUnique a1 a2)) c
andActivator f g v this hdl exec =
    f v this (F.lfilterHandler reinterpret' hdl) exec >>
    g v this (F.lfilterHandler reinterpret' hdl) exec

wack
    :: (UniqueMember [R.Listener] s')
    => J.JSString
    -> (J.JSVal -> IO a)
    -> IORef s
    -> (s -> m (Many s'))
    -> ((Many s') -> m s)
    -> F.Handler m (Many s') a c
    -> F.Executor (DL.DList c)
    -> m ()
wack n trig v deref ref hdl exec = do
    cb <- R.mkCallback trig goLazy exec
    s <- R.doReadIORef v
    s' <- deref s
    -- R.doModifyIORef' (item %~ (`DL.snoc` (n, cb))) v
  where
    goLazy = undefined
