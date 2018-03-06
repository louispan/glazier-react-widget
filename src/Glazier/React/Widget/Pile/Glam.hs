{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Glazier.React.Widget.Pile.Glam where

import Control.Lens
import qualified Control.Monad.ListM as LM
import Control.Monad.Reader
import Data.Foldable
import Data.Generics.Product
import qualified GHC.Generics as G
import Glazier.React.Framework
import Glazier.React.Widget.Pile as W

-- | Contains information on sorting and filtering the items in a pile
-- differerently from the native data structure.
data GlamPile flt srt t a = GlamPile
    { filterCriteria :: flt
    , sortCriteria :: srt
    , glamList :: [a] -- filtered and sorted
    , rawPile :: t a
    } deriving (G.Generic, Functor)

-- | Converts a builder with a plan of @[a]@ to a plan of @Mapping a@
glamPileBuilder :: (Traversable t, Applicative m)
    => Builder r s m r' s'
    -> Builder (GlamPile flt srt t r) (GlamPile flt srt t s)
        m (GlamPile flt srt t r') (GlamPile flt srt t s')
glamPileBuilder (Builder (ReaderT mkReq) (ReaderT mkSpc)) =
    Builder (ReaderT mkReq') (ReaderT mkSpc')
  where
    mkReq' (GlamPile fc sc al ss) = GlamPile fc sc <$> traverse mkReq al <*> traverse mkReq ss
    mkSpc' (GlamPile fc sc al rs) = GlamPile fc sc <$> traverse mkSpc al <*> traverse mkSpc rs

glamPile :: (Traversable t, MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Archetype s m c
    -> Prototype p (GlamPile flt srt t s) m c
glamPile flt srt (Archetype dis fin ini)
    = Prototype
        (glamPileDisplay flt srt dis)
        (glamPileFinalizer fin)
        (glamPileInitializer ini)

-- | Sort the items on the listing given a sorting function
hdlGlamPileSortCriteria :: (MonadReactor m)
    => srt -> Delegate (Scene p m (GlamPile flt srt t a)) m ()
hdlGlamPileSortCriteria f = delegate' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self $ \me ->
        me & (my._model.field @"sortCriteria" .~ f)
            . (my._model.field @"glamList" .~ []) -- this tells render to update displayItems
    dirty this

-- | Filter the items on the listing given a filter function
hdlGlamPileFilterCriteria :: (MonadReactor m)
    => flt -> Delegate (Scene p m (GlamPile flt srt t a)) m ()
hdlGlamPileFilterCriteria f = delegate' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self $ \me ->
        me & (my._model.field @"filterCriteria" .~ f)
            . (my._model.field @"glamList" .~ []) -- this tells render to update displayItems
    dirty this

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastGlamPileHandler :: (Traversable t, MonadReactor m)
    => (a -> Delegate s m b)
    -> a -> Delegate (Scene p m (GlamPile flt srt t s)) m b
broadcastGlamPileHandler hdl a = magnify (editScene (field @"rawPile")) (W.broadcastPileHandler hdl a)

glamPileDisplay :: (Foldable t, MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Display s m ()
    -> FrameDisplay (GlamPile flt srt t s) m ()
glamPileDisplay flt srt dis = method' $ \(Frame _ (GlamPile df ds ys xs)) -> do
    let toLi s = bh "li" [] (runMethod' dis s)
        df' = flt df
        ds' = srt ds
    ys' <- lift $ case ys of
            -- if displayList is empty, then run the filter and sort
            [] -> do
                let zs = toList xs
                zs' <- LM.filterMP df' zs
                zs'' <- LM.sortByM ds' zs'
                pure zs''
            -- else display as is
            ys' -> pure ys'
    bh "ul" [] (fold $ toLi <$> ys')

glamPileFinalizer :: (Traversable t, Monad m)
    => Finalizer s m -> Finalizer (GlamPile flt srt t s) m
glamPileFinalizer fin = magnify (field @"rawPile") (W.pileFinalizer fin)

glamPileInitializer :: (Foldable t, MonadReactor m)
    => Delegate s m b
    -> Delegate (Scene p m (GlamPile flt srt t s)) m b
glamPileInitializer ini = magnify (editScene (field @"rawPile")) (W.pileInitializer ini)
