{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.Pile where

import Control.Lens
import Data.Foldable
import qualified Data.Foldable.Esoteric as E
import Glazier.React.Framework

-- | Converts a builder to a builder that can build a list (or any Traversable)
pileBuilder :: (Applicative m, Traversable t)
    => Builder r s m r' s'
    -> Builder (t r) (t s) m (t r') (t s')
pileBuilder (Builder mkReq mkSpc) =
    Builder mkReq' mkSpc'
  where
    mkReq' = traverse mkReq
    mkSpc' = traverse mkSpc

pile :: (Traversable t, MonadReactor m)
    => Archetype s m c
    -> Prototype p (t s) m c
pile (Archetype dis fin ini)
    = Prototype
        (pileDisplay dis)
        (pileFinalizer fin)
        (pileInitializer ini)

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastPileHandler :: (Foldable t, MonadReactor m)
    => (a -> MethodT s m b)
    -> a -> MethodT (Scene p m (t s)) m b
broadcastPileHandler hdl a = methodT' $ \Obj{..} fire -> do
    me <- doReadIORef self
    E.traverse_' (\s -> runMethodT' (hdl a) s fire) (me ^. my._model)

pileFinalizer :: (Traversable t, Applicative m)
    => Finalizer s m -> Finalizer (t s) m
pileFinalizer fin ss = fold <$> traverse fin ss

pileDisplay :: (Functor t, Foldable t, Monad m)
    => Display s m ()
    -> FrameDisplay (t s) m ()
pileDisplay dis (Frame _ ss) = do
    let toLi s = bh "li" [] (dis s)
    bh "ul" [] (fold $ toLi <$> ss)

pileInitializer :: (Foldable t, MonadReactor m)
    => MethodT s m b
    -> MethodT (Scene p m (t s)) m b
pileInitializer ini = methodT' $ \Obj{..} fire -> do
    me <- doReadIORef self
    E.traverse_' (\s -> runMethodT' ini s fire) (me ^. my._model)
