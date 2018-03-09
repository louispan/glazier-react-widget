{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.Pile.Glam where

import Control.Arrow
import Control.Lens
import Control.Lens.Misc
import qualified Control.Monad.ListM as LM
import Control.Monad.Reader
import Data.Foldable
import qualified GHC.Generics as G
import Glazier.React.Framework
import Glazier.React.Widget.Pile as W

-- | Contains information on sorting and filtering the items in a pile
-- differerently from the native data structure.
data GlamPile ftr srt t a = GlamPile
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , glamList :: [a] -- filtered and sorted
    , rawPile :: t a
    } deriving (G.Generic, Functor)

makeLenses_ ''GlamPile

-- | Converts a builder with a plan of @[a]@ to a plan of @Mapping a@
glamPileBuilder :: (Traversable t, Applicative m)
    => Builder r s m r' s'
    -> Builder (GlamPile ftr srt t r) (GlamPile ftr srt t s)
        m (GlamPile ftr srt t r') (GlamPile ftr srt t s')
glamPileBuilder (Builder mkReq mkSpc) =
    Builder mkReq' mkSpc'
  where
    mkReq' (GlamPile fc sc al ss) = GlamPile fc sc <$> traverse mkReq al <*> traverse mkReq ss
    mkSpc' (GlamPile fc sc al rs) = GlamPile fc sc <$> traverse mkSpc al <*> traverse mkSpc rs

glamPile :: (Traversable t, MonadReactor m)
    => (ftr -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Archetype s m c
    -> Prototype p (GlamPile ftr srt t s) m c
glamPile ftr srt (Archetype dis fin ini)
    = Prototype
        (glamPileDisplay ftr srt dis)
        (glamPileFinalizer fin)
        (glamPileInitializer ini)

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastGlamPileHandler :: (Traversable t, MonadReactor m)
    => (a -> MethodT s m b)
    -> a -> MethodT (Scene p m (GlamPile ftr srt t s)) m b
broadcastGlamPileHandler hdl a = magnify (to $ accessScene _rawPile) (W.broadcastPileHandler hdl a)

-- | Sort the items on the listing given a sorting function
hdlGlamPileSortCriteria :: (MonadReactor m)
    => srt -> MethodT (Scene p m (GlamPile ftr srt t s)) m ()
hdlGlamPileSortCriteria f = readrT' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self
        $ my._model._sortCriteria .~ f
        >>> my._model._glamList .~ [] -- this tells render to update displayItems
    dirty this

-- | Filter the items on the listing given a filter function
hdlGlamPileFilterCriteria :: (MonadReactor m)
    => ftr -> MethodT (Scene p m (GlamPile ftr srt t s)) m ()
hdlGlamPileFilterCriteria f = readrT' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self
        $ my._model._filterCriteria .~ f
        >>> my._model._glamList .~ [] -- this tells render to update displayItems
    dirty this

glamPileDisplay :: (Foldable t, MonadReactor m)
    => (ftr -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Display s m ()
    -> FrameDisplay (GlamPile ftr srt t s) m ()
glamPileDisplay ftr srt dis (Frame _ (GlamPile df ds ys xs)) = do
    let toLi s = bh "li" [] (dis s)
        df' = ftr df
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
    => Finalizer s m -> Finalizer (GlamPile ftr srt t s) m
glamPileFinalizer fin = magnify _rawPile (W.pileFinalizer fin)

glamPileInitializer :: (Foldable t, MonadReactor m)
    => MethodT s m b
    -> MethodT (Scene p m (GlamPile ftr srt t s)) m b
glamPileInitializer ini = magnify (to $ accessScene _rawPile) (W.pileInitializer ini)
