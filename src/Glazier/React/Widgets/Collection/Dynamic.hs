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
    -> ModelState (DynamicCollection ftr srt k s Obj) ()
updateVisibleList ff fs = do
    zs@(DynamicCollection ftr srt _ xs) <- use id
    let xs' = toList xs
        ftr' x = do
            x' <- doReadIORef $ modelRef x
            ff ftr (model x')
        srt' x y = do
            x' <- doReadIORef $ modelRef x
            y' <- doReadIORef $ modelRef y
            fs srt (model x') (model y')
    ys <- lift $ LM.filterMP ftr' xs' >>= LM.sortByM srt'
    id .= zs { visibleList = ys }

dynamicCollectionWindow :: ReactId -> Window (DynamicCollection ftr srt k s Obj) ()
dynamicCollectionWindow ri = magnifiedModel _visibleList $ collectionWindow ri
