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

module Glazier.React.Widgets.Collection where

import Control.Lens
import Data.Foldable
-- import qualified Data.Foldable.Esoteric as E
import Glazier.React

-- -- | lift a handler for a single gadget into a handler of a list of gadgets
-- -- where the input is broadcast to all the items in the list.
-- broadcastPileHandler :: (Foldable t, MonadReactor m)
--     => (a -> Gadget c p s b)
--     -> a -> Gadget c p (t s) m b
-- broadcastPileHandler hdl a = methodT' $ \Obj{..} fire -> do
--     me <- doReadIORef self
--     E.traverse_' (\s -> runMethodT' (hdl a) s fire) (me ^. my._model)

-- Given a Foldable of items that have been removed from a Collection model
-- schedule cleanup of the callbacks
cleanupCollectionOnRendered :: Foldable t => t (Subject s) -> Subject c -> ReactorCmd cmd
cleanupCollectionOnRendered ss sbj =
    let cleanup = foldr ((>>) . prolong) (pure ()) ss
    in TickScene sbj $ _plan._doOnRendered._once %= (*> cleanup)

-- pileFinalizer :: (Traversable t, Applicative m)
--     => Finalizer s m -> Finalizer (t s) m
-- pileFinalizer fin ss = fold <$> traverse fin ss

displayCollection :: (MonadIO m, Functor t, Foldable t)
    => WindowT (t (Subject s)) m ()
displayCollection = do
    ss <- view _model
    let toLi s = branch "li" [] (displaySubject s)
    branch "ul" [] (fold $ toLi <$> ss)

-- pileInitializer :: (Foldable t, MonadReactor m)
--     => Gadget c p s b
--     -> Gadget c p (t s) b
-- pileInitializer ini = methodT' $ \Obj{..} fire -> do
--     me <- doReadIORef self
--     E.traverse_' (\s -> runMethodT' ini s fire) (me ^. my._model)
