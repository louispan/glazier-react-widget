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
    , dynamicCollectionWindow
    , updateVisibleList
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
data DynamicCollection ftr srt k a = DynamicCollection
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , visibleList :: [Obj a] -- filtered and sorted.
    , rawCollection :: M.Map k (Obj a)
    } deriving (G.Generic)

makeLenses_ ''DynamicCollection

updateVisibleList ::
    (ftr -> s -> Benign IO Bool)
    -> (srt -> s -> s -> Benign IO Ordering)
    -> ModelState (DynamicCollection ftr srt k s) ()
updateVisibleList ff fs = do
    zs@(DynamicCollection ftr srt _ xs) <- use id
    let xs' = toList xs
        ftr' x = do
            x' <- benignReadIORef $ modelRef x
            ff ftr (model x')
        srt' x y = do
            x' <- benignReadIORef $ modelRef x
            y' <- benignReadIORef $ modelRef y
            fs srt (model x') (model y')
    ys <- lift $ LM.filterMP ftr' xs' >>= LM.sortByM srt'
    id .= zs { visibleList = ys }

dynamicCollectionWindow :: ReactId -> Window (DynamicCollection ftr srt k s) ()
dynamicCollectionWindow k = magnifiedModel _visibleList $ collectionWindow k
