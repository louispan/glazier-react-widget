{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widgets.Collection.Dynamic
    ( DynamicCollection(..)
    , _filterCriteria
    , _sortCriteria
    , _visibleList
    , _rawCollection
    , dynamicCollectionDisplay
    , regenerateVisibleList
    , hdlDynamicCollectionSortCriteria
    , hdlDynamicCollectionFilterCriteria
    , hdlDynamicCollectionDeleteItem
    , hdlDynamicCollectionInsertItem
    ) where

import Control.Lens
import Control.Lens.Misc
import qualified Control.Monad.ListM as LM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Widgets.Collection as W

-- | Contains information on sorting and filtering the items in a collection
-- differerently from the native data structure.
data DynamicCollection ftr srt k a = DynamicCollection
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , visibleList :: [a] -- filtered and sorted. If empty, it will be generated on rerender
    , rawCollection :: M.Map k a
    } deriving (G.Generic, Functor, Eq, Ord, Show)

makeLenses_ ''DynamicCollection

regenerateVisibleList ::
    (ftr -> s -> Bool)
    -> (srt -> s -> s -> Ordering)
    -> StateT (DynamicCollection ftr srt k (Subject s)) ReadIORef ()
regenerateVisibleList ff fs = do
    zs@(DynamicCollection ftr srt _ xs) <- get
    let xs' = toList xs
        toSbj = sceneRef
        ftr' x = (\x' -> ff ftr (model x')) <$> (doReadIORef $ toSbj x)
        srt' x y = (\x' y' -> fs srt (model x') (model y')) <$> (doReadIORef $ toSbj x) <*> (doReadIORef $ toSbj y)
    ys <- lift $ LM.filterMP ftr' xs' >>= LM.sortByM srt'
    put $ zs { visibleList = ys }

-- | Sort the items on the listing given a sorting function
hdlDynamicCollectionSortCriteria ::
    ( MonadReactor p (DynamicCollection ftr srt k (Subject s)) cmd m
    )
    => (ftr -> s -> Bool)
    -> (srt -> s -> s -> Ordering)
    -> srt
    -> m ()
hdlDynamicCollectionSortCriteria ff fs srt = tickScene $ do
    _model._sortCriteria .= srt
    zoom _model $ regenerateVisibleList ff fs

-- | Filter the items on the listing given a filter function
hdlDynamicCollectionFilterCriteria ::
    ( MonadReactor p (DynamicCollection ftr srt k (Subject s)) cmd m
    )
    => (ftr -> s -> Bool)
    -> (srt -> s -> s -> Ordering)
    -> ftr
    -> m ()
hdlDynamicCollectionFilterCriteria ff fs ftr = tickScene $ do
    _model._filterCriteria .= ftr
    zoom _model $ regenerateVisibleList ff fs

dynamicCollectionDisplay :: Window (DynamicCollection ftr srt k (Subject s)) ()
dynamicCollectionDisplay = magnify (editSceneModel _visibleList) collectionDisplay

cleanupDynamicCollectionItem :: (MonadState (Scene (DynamicCollection ftr srt k (Subject s))) m, Ord k)
    => k -> MaybeT m ()
cleanupDynamicCollectionItem k = do
    old <- MaybeT $ use (_model._rawCollection.at k)
    cleanupSubject old

hdlDynamicCollectionDeleteItem ::
    (MonadReactor p (DynamicCollection ftr srt k (Subject s)) cmd m, Ord k)
    => (ftr -> s -> Bool)
    -> (srt -> s -> s -> Ordering)
    -> k
    -> m ()
hdlDynamicCollectionDeleteItem ff fs k =
    tickScene $ void $ runMaybeT $ do
        cleanupDynamicCollectionItem k
        _model._rawCollection.at k .= Nothing
        lift $ zoom _model $ regenerateVisibleList ff fs

hdlDynamicCollectionInsertItem ::
    (MonadReactor p (DynamicCollection ftr srt k (Subject s)) cmd m, Ord k)
    => (ftr -> s -> Bool)
    -> (srt -> s -> s -> Ordering)
    -> Widget cmd s s a
    -> k
    -> s
    -> m a
hdlDynamicCollectionInsertItem ff fs wid k s = mkSubject wid s $ \sbj ->
    tickScene $ void $ runMaybeT $ do
        cleanupDynamicCollectionItem k
        _model._rawCollection.at k .= Just sbj
        lift $ zoom _model $ regenerateVisibleList ff fs
