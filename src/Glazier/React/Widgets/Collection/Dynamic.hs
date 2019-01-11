{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widgets.Collection.Dynamic where

import Control.Applicative
import Control.Lens
import Control.Lens.Misc
import qualified Control.Monad.ListM as LM
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Applicative as A
import qualified Data.Aeson.Encoding as AE
import Data.Foldable
import Data.Functor.Compose
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Widgets.Collection

-- | Contains information on sorting and filtering the items in a collection
-- differerently from the native data structure.
data DynamicCollection ftr srt k s = DynamicCollection
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , visibleList :: [Obj s] -- filtered and sorted.
    , rawCollection :: M.Map k (Obj s)
    } deriving (G.Generic)

makeLenses_ ''DynamicCollection

instance
    ( A.ToJSONKey k
    , A.AToJSON m (Obj s)
    , A.AToJSON m ftr
    , A.AToJSON m srt
    )
    => A.AToJSON m (DynamicCollection ftr srt k s) where
    atoEncoding DynamicCollection {..} = A.pairs <$> filterCriteria' `combine` sortCriteria' `combine` rawCollection'
      where
        combine = liftA2 (<>)
        filterCriteria' = AE.pair "filterCriteria" <$> A.atoEncoding filterCriteria
        sortCriteria' = AE.pair "sortCriteria" <$> A.atoEncoding sortCriteria
        rawCollection' = AE.pair "rawCollection" <$> A.atoEncoding rawCollection

-- FIXME: updateVisibleList
instance
    ( FilterPredicate ftr s
    , SortPredicate srt s
    , MonadReactor c m
    , A.FromJSONKey k
    , Ord k
    , A.AFromJSON m (Obj s)
    , A.AFromJSON m ftr
    , A.AFromJSON m srt
    )
    => A.AFromJSON m (DynamicCollection ftr srt k s) where
    aparseJSON = A.withObject "DynamicCollection" $ \v -> fmap go $ getCompose $ DynamicCollection
        <$> (Compose $ A.aparseField v "filterCriteria")
        <*> (Compose $ A.aparseField v "sortCriteria")
        <*> (pure mempty)
        <*> (Compose $ A.aparseField v "rawCollection")
      where
        go ::
            ( FilterPredicate ftr s
            , SortPredicate srt s
            , MonadReactor c m
            ) => m (DynamicCollection ftr srt k s) -> m (DynamicCollection ftr srt k s)
        go m = do
            s <- m
            evalBenignIO (execStateT updateVisibleList s)

class FilterPredicate ftr s where
    filterPredicate :: ftr -> s -> Benign IO Bool

class SortPredicate srt s where
    sortPredicate :: srt -> s -> s -> Benign IO Ordering

updateVisibleList ::
    ( FilterPredicate ftr s
    , SortPredicate srt s
    ) => ModelState (DynamicCollection ftr srt k s) ()
updateVisibleList = do
    zs@(DynamicCollection ftr srt _ xs) <- use id
    let xs' = toList xs
        ftr' x = do
            x' <- benignReadIORef $ modelRef x
            filterPredicate ftr (model x')
        srt' x y = do
            x' <- benignReadIORef $ modelRef x
            y' <- benignReadIORef $ modelRef y
            sortPredicate srt (model x') (model y')
    ys <- lift $ LM.filterMP ftr' xs' >>= LM.sortByM srt'
    id .= zs { visibleList = ys }

dynamicCollectionWindow :: ReactId -> Window (DynamicCollection ftr srt k s) ()
dynamicCollectionWindow k = magnifiedModel _visibleList $ collectionWindow k
