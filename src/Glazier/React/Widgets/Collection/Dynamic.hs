{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

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
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Widgets.Collection

-- | Contains information on sorting and filtering the items in a collection
-- differerently from the native data structure.
data DynamicCollection ftr srt k s = DynamicCollection
    { filterCriteria :: ftr
    , sortCriteria :: srt
    , visibleList :: [s] -- filtered and sorted.
    , rawCollection :: M.Map k s
    } deriving (G.Generic, Functor)

makeLenses_ ''DynamicCollection

instance
    ( A.ToJSONKey k
    , A.AToJSON m s
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

-- instance
--     ( A.FromJSONKey k
--     , Ord k
--     , A.AFromJSON m s
--     , A.AFromJSON m ftr
--     , A.AFromJSON m srt
--     , MonadReactor c m
--     )
--     => A.AFromJSON (ReaderT (ftr -> s -> Benign IO Bool, srt -> s -> s -> Benign IO Ordering) m) (DynamicCollection ftr srt k s) where
--     aparseJSON = A.withObject "DynamicCollection" $ \v -> fmap go $ getCompose $ DynamicCollection
--             <$> (Compose $ lift <$> A.aparseField v "filterCriteria")
--             <*> (Compose $ lift <$> A.aparseField v "sortCriteria")
--             <*> (pure mempty)
--             <*> (Compose $ lift <$> A.aparseField v "rawCollection")
--       where
--         go m = do
--             (fftr, fsrt) <- ask
--             s <- m
--             lift $ evalBenignIO (execStateT (updateVisibleList fftr fsrt) s)

-- instance
--     ( A.FromJSONKey k
--     , Ord k
--     , A.AFromJSON m ftr
--     , A.AFromJSON m srt
--     , MonadReactor c m
--     )
--     => A.AFromJSON1 (ReaderT (ftr -> s -> Benign IO Bool, srt -> s -> s -> Benign IO Ordering) m) (DynamicCollection ftr srt k) where
--     aliftParseJSON p _ = A.withObject "DynamicCollection" $ \v -> fmap go $ getCompose $ DynamicCollection
--             <$> (Compose $ lift <$> A.aparseField v "filterCriteria")
--             <*> (Compose $ lift <$> A.aparseField v "sortCriteria")
--             <*> (pure mempty)
--             <*> (Compose $ lift <$> A.aparseField v "rawCollection")
--       where
--         go m = do
--             (fftr, fsrt) <- ask
--             s <- m
--             lift $ evalBenignIO (execStateT (updateVisibleList fftr fsrt) s)

updateVisibleList ::
    (ftr -> s -> Benign IO Bool)
    -> (srt -> s -> s -> Benign IO Ordering)
    -> SceneState (DynamicCollection ftr srt k s) ()
updateVisibleList fftr fsrt = do
    zs@(DynamicCollection ftr srt _ xs) <- use id
    let xs' = toList xs
    ys <- lift $ LM.filterMP (fftr ftr) xs' >>= LM.sortByM (fsrt srt)
    id .= zs { visibleList = ys }

-- dynamicCollectionWindow :: ReactId -> Window (DynamicCollection ftr srt k (Obj s)) ()
-- dynamicCollectionWindow k = magnifiedModel _visibleList $ collectionWindow k

dynamicCollectionWindow :: Traversal' s (DynamicCollection ftr srt k s') -> (s' -> Window s ()) -> Window s ()
dynamicCollectionWindow this = collectionWindow (this._visibleList)
