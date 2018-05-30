{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widgets.Collection.Dynamic
    ( DynamicCollection(..)
    , _filterCriteria
    , _sortCriteria
    , _visibleList
    , _rawCollection
    , dynamicCollectionDisplay
    , regenerateVisibleList
    , setDynamicCollectionSortCriteria
    , setDynamicCollectionFilterCriteria
    , deleteDynamicCollectionItem
    , insertDynamicCollectionItem
    , module Glazier.React.Widgets.Collection
    ) where

import Control.Lens
import Control.Lens.Misc
import qualified Control.Monad.ListM as LM
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Widgets.Collection

-- | Contains information on sorting and filtering the items in a collection
-- differerently from the native data structure.
data DynamicCollection ftr srt k f a = DynamicCollection
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , visibleList :: [HKD f a] -- filtered and sorted. If empty, it will be generated on rerender
    , rawCollection :: M.Map k (HKD f a)
    } deriving (G.Generic)

makeLenses_ ''DynamicCollection

regenerateVisibleList ::
    (ftr -> s -> ReadIORef Bool)
    -> (srt -> s -> s -> ReadIORef Ordering)
    -> SceneState (DynamicCollection ftr srt k Subject s) ()
regenerateVisibleList ff fs = do
    zs@(DynamicCollection ftr srt _ xs) <- use _model
    let xs' = toList xs
        toSbj = sceneRef
        ftr' x = do
            x' <- doReadIORef $ toSbj x
            ff ftr (model x')
        srt' x y = do
            x' <- doReadIORef $ toSbj x
            y' <- doReadIORef $ toSbj y
            fs srt (model x') (model y')
    ys <- lift $ LM.filterMP ftr' xs' >>= LM.sortByM srt'
    _model .= zs { visibleList = ys }

-- | Sort the items on the listing given a sorting function
setDynamicCollectionSortCriteria ::
    (ftr -> s -> ReadIORef Bool)
    -> (srt -> s -> s -> ReadIORef Ordering)
    -> srt
    -> SceneState (DynamicCollection ftr srt k Subject s) ()
setDynamicCollectionSortCriteria ff fs srt = do
    _model._sortCriteria .= srt
    regenerateVisibleList ff fs

-- | Filter the items on the listing given a filter function
setDynamicCollectionFilterCriteria ::
    (ftr -> s -> ReadIORef Bool)
    -> (srt -> s -> s -> ReadIORef Ordering)
    -> ftr
    -> SceneState (DynamicCollection ftr srt k Subject s) ()
setDynamicCollectionFilterCriteria ff fs ftr = do
    _model._filterCriteria .= ftr
    regenerateVisibleList ff fs

dynamicCollectionDisplay :: Window (DynamicCollection ftr srt k Subject s) ()
dynamicCollectionDisplay = magnify (editSceneModel _visibleList) collectionDisplay

deleteDynamicCollectionItem :: (Ord k)
    => (ftr -> s -> ReadIORef Bool)
    -> (srt -> s -> s -> ReadIORef Ordering)
    -> k
    -> MaybeT (SceneState (DynamicCollection ftr srt k Subject s)) ()
deleteDynamicCollectionItem ff fs k = do
    zoom (editSceneModel _rawCollection) (deleteCollectionItem k)
    lift $ regenerateVisibleList ff fs

insertDynamicCollectionItem :: (Ord k)
    => (ftr -> s -> ReadIORef Bool)
    -> (srt -> s -> s -> ReadIORef Ordering)
    -> k
    -> Subject s
    -> SceneState (DynamicCollection ftr srt k Subject s) ()
insertDynamicCollectionItem ff fs k sbj = do
    zoom (editSceneModel _rawCollection) (insertCollectionItem k sbj)
    regenerateVisibleList ff fs
