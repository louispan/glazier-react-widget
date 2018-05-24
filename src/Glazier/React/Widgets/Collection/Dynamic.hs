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

module Glazier.React.Widgets.Collection.Dynamic where

import Control.Lens
import Control.Lens.Misc
import qualified Control.Monad.ListM as LM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Widgets.Collection as W

-- | Contains information on sorting and filtering the items in a pile
-- differerently from the native data structure.
data DynamicCollection ftr srt t a = DynamicCollection
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , visibleList :: [a] -- filtered and sorted. If empty, it will be generated on rerender
    , rawCollection :: t a
    } deriving (G.Generic, Functor, Eq, Ord, Show)

makeLenses_ ''DynamicCollection

regenerateVisibleList :: Foldable t =>
    (ftr -> s -> Bool)
    -> (srt -> s -> s -> Ordering)
    -> StateT (DynamicCollection ftr srt t (Subject s)) ReadIORef ()
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
    ( Foldable t
    , MonadReactor p (DynamicCollection ftr srt t (Subject s)) cmd m
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
    ( Foldable t
    , MonadReactor p (DynamicCollection ftr srt t (Subject s)) cmd m
    )
    => (ftr -> s -> Bool)
    -> (srt -> s -> s -> Ordering)
    -> ftr
    -> m ()
hdlDynamicCollectionFilterCriteria ff fs ftr = tickScene $ do
    _model._filterCriteria .= ftr
    zoom _model $ regenerateVisibleList ff fs

dynamicCollectionDisplay :: Window (DynamicCollection ftr srt t (Subject s)) ()
dynamicCollectionDisplay = magnify (editSceneModel _visibleList) collectionDisplay
