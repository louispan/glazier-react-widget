{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widgets.Collection.Dynamic
    ( DynamicCollection(..)
    , HKDynamicCollection
    , _filterCriteria
    , _sortCriteria
    , _visibleList
    , _rawCollection
    , dynamicCollectionWindow
    , updateVisibleList
    -- , setDynamicCollectionSortCriteria
    -- , setDynamicCollectionFilterCriteria
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
data DynamicCollection ftr srt k a f = DynamicCollection
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , visibleList :: [HKD f a] -- filtered and sorted.
    , rawCollection :: M.Map k (HKD f a)
    } deriving (G.Generic)

type HKDynamicCollection ftr srt k a f = DynamicCollection ftr srt k (a f) f

makeLenses_ ''DynamicCollection

updateVisibleList ::
    (ftr -> s -> ReadIORef Bool)
    -> (srt -> s -> s -> ReadIORef Ordering)
    -> ModelState (DynamicCollection ftr srt k s Subject) ()
updateVisibleList ff fs = do
    zs@(DynamicCollection ftr srt _ xs) <- use id
    let xs' = toList xs
        ftr' x = do
            x' <- doReadIORef $ sceneRef x
            ff ftr (model x')
        srt' x y = do
            x' <- doReadIORef $ sceneRef x
            y' <- doReadIORef $ sceneRef y
            fs srt (model x') (model y')
    ys <- lift $ LM.filterMP ftr' xs' >>= LM.sortByM srt'
    id .= zs { visibleList = ys }

-- -- | Sort the items on the listing given a sorting function
-- setDynamicCollectionSortCriteria ::
--     (ftr -> s -> ReadIORef Bool)
--     -> (srt -> s -> s -> ReadIORef Ordering)
--     -> srt
--     -> SceneState (DynamicCollection ftr srt k s Subject) ()
-- setDynamicCollectionSortCriteria ff fs srt = do
--     _model._sortCriteria .= srt
--     regenerateVisibleList ff fs

-- -- | Filter the items on the listing given a filter function
-- setDynamicCollectionFilterCriteria ::
--     (ftr -> s -> ReadIORef Bool)
--     -> (srt -> s -> s -> ReadIORef Ordering)
--     -> ftr
--     -> SceneState (DynamicCollection ftr srt k s Subject) ()
-- setDynamicCollectionFilterCriteria ff fs ftr = do
--     _model._filterCriteria .= ftr
--     regenerateVisibleList ff fs

dynamicCollectionWindow :: ReactId -> Window (DynamicCollection ftr srt k s Subject) ()
dynamicCollectionWindow ri = magnifiedScene _visibleList $ collectionWindow ri

deleteDynamicCollectionItem :: (MonadReactor p allS cmd m, Ord k)
    => k
    -> ModelState (DynamicCollection ftr srt k s Subject) (m ())
deleteDynamicCollectionItem k =
    zoom _rawCollection (deleteCollectionItem k)
    -- lift $ regenerateVisibleList ff fs

insertDynamicCollectionItem :: (MonadReactor p allS cmd m, Ord k)
    => k
    -> Subject s
    -> ModelState (DynamicCollection ftr srt k s Subject) (m ())
insertDynamicCollectionItem k sbj =
    zoom _rawCollection (insertCollectionItem k sbj)
    -- regenerateVisibleList ff fs
