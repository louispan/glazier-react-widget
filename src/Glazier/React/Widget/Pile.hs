{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.Pile where

import Control.Lens
import Control.Monad.Trans.Cont
import Data.Foldable
import qualified Data.Foldable.Esoteric as E
import qualified Glazier.React.Framework as Z

-- | Converts a builder to a builder that can build a list (or any Traversable)
pileBuilder :: (Applicative m, Traversable t)
    => Z.Builder m r s r' s'
    -> Z.Builder m (t r) (t s) (t r') (t s')
pileBuilder (Z.Builder (Z.MkReq mkReq, Z.MkSpec mkSpc)) =
    Z.Builder (Z.MkReq mkReq', Z.MkSpec mkSpc')
  where
    mkReq' = traverse mkReq
    mkSpc' = traverse mkSpc

pile :: (Traversable t, Z.MonadReactor m)
    => Z.Archetype m s c
    -> Z.Prototype m v (t s) c
pile (Z.Archetype dis fin ini)
    = Z.Prototype
        (pileDisplay dis)
        (pileFinalizer fin)
        (pileInitializer ini)

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastPileHandler :: (Foldable t, Z.MonadReactor m)
    => Z.Handler m s a b
    -> Z.SceneHandler m v (t s) a b
broadcastPileHandler hdl (Z.Obj ref its) a = ContT $ \fire -> do
    obj <- Z.doReadIORef ref
    E.traverse_' (\s -> runContT (hdl s a) fire) (obj ^. its.Z.model)

pileFinalizer :: (Traversable t, Applicative m)
    => Z.Finalizer m s -> Z.Finalizer m (t s)
pileFinalizer fin ss = fold <$> traverse fin ss

pileDisplay :: (Functor t, Foldable t, Monad m)
    => Z.Display m s ()
    -> Z.FrameDisplay m (t s) ()
pileDisplay dis (_, ss) = do
    let toLi s = Z.bh "li" [] (dis s)
    Z.bh "ul" [] (fold $ toLi <$> ss)

pileInitializer :: (Foldable t, Z.MonadReactor m)
    => Z.Initializer m s b
    -> Z.SceneInitializer m v (t s) b
pileInitializer ini (Z.Obj ref its) = ContT $ \k -> do
    obj <- Z.doReadIORef ref
    E.traverse_' (\s -> runContT (ini s) k) (obj ^. its.Z.model)
