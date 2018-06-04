-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widgets.Collection
    ( HKD
    , UKey
    , zeroUKey
    , smallerUKey
    , largerUKey
    , betweenUKey
    -- * Collection
    , Collection
    , HKCollection
    , collectionDisplay
    , deleteCollectionItem
    , insertCollectionItem
    ) where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React
import JavaScript.Extras.Number

-- "Higher-Kinded Data" http://reasonablypolymorphic.com/blog/higher-kinded-data/
-- Erases Identity
type family HKD f a where
    HKD Identity a = a
    HKD f        a = f a

-- | A key where you can always create
-- another key ordered between two different keys,
-- or another key above or below this key.
-- Memonic: U for uncountable https://en.wikipedia.org/wiki/Uncountable_set
newtype UKey = UKey { unUKey :: [Int] }
    deriving (G.Generic, Show)

-- | For comparison purposes, an empty list is equivalent to [0,0,..]
instance Ord UKey where
    compare (UKey []) (UKey []) = EQ
    compare (UKey xs) (UKey []) = compare (UKey xs) (UKey [0])
    compare (UKey []) (UKey ys) = compare (UKey [0]) (UKey ys)
    compare (UKey (x : xs)) (UKey (y : ys)) = case compare x y of
        EQ -> compare (UKey xs) (UKey ys)
        o -> o

-- | For comparison purposes, an empty list is equivalent to [0,0,..]
instance Eq UKey where
    (UKey []) == (UKey []) = True
    (UKey xs) == (UKey []) = (UKey xs) == (UKey [0])
    (UKey []) == (UKey ys) = (UKey [0]) == (UKey ys)
    (UKey (x : xs)) == (UKey (y : ys)) = if x == y
        then (UKey xs) == (UKey ys)
        else False

zeroUKey :: UKey
zeroUKey = UKey []

ukeyStep :: Int
ukeyStep = 32

-- | Create a key smaller than the input key.
smallerUKey :: UKey -> UKey
smallerUKey (UKey []) = UKey [-ukeyStep]
smallerUKey (UKey (a : as)) = UKey $ case compare a (minSafeInteger + ukeyStep) of
        LT -> if minSafeInteger == a
            then minSafeInteger : unUKey (smallerUKey (UKey as))
            else [minSafeInteger]
        _ -> [a - ukeyStep]

-- | Create a key larger than the input key.
largerUKey :: UKey -> UKey
largerUKey (UKey []) = UKey [ukeyStep]
largerUKey (UKey (a : as)) = UKey $ case compare a (maxSafeInteger - ukeyStep) of
        GT -> if maxSafeInteger == a
            then maxSafeInteger : unUKey (largerUKey (UKey as))
            else [maxSafeInteger]
        _ -> [a + ukeyStep]

-- | Make a key that will fit in between the two provided keys,
-- with no guarantees on how close it is to the mid point.
-- Except when the inputs are equal, then it will return the same key.
betweenUKey :: UKey -> UKey -> UKey
betweenUKey (UKey []) (UKey []) = zeroUKey
betweenUKey (UKey xs) (UKey []) = betweenUKey (UKey xs) (UKey [0])
betweenUKey (UKey []) (UKey ys) = betweenUKey (UKey [0]) (UKey ys)
betweenUKey (UKey (x : xs)) (UKey (y : ys)) = UKey $ case compare x y of
    LT -> if x + 1 == y
        then x : unUKey (betweenUKey (UKey xs) (UKey $ repeat maxSafeInteger))
        else [betweenUncInt x y]
    GT -> if y + 1 == x
        then y : unUKey (betweenUKey (UKey ys) (UKey $ repeat maxSafeInteger))
        else [betweenUncInt x y]
    EQ -> x : unUKey (betweenUKey (UKey xs) (UKey ys))

betweenUncInt :: Int -> Int -> Int
betweenUncInt x y =
    let (xq, xr) = quotRem x 2
        (yq, yr) = quotRem y 2
        z = case (xr + yr) of
            2 -> 1
            _ -> 0
    in xq + yq + z

-----------------------------------------------------------------

-- | Collection of higher kinded data
type Collection t s f = t (HKD f s)

-- | Collection of higher kinded "higher kinded data"
type HKCollection t s f = t (HKD f (s f))

-- | Collection doesn't have an initializing gadget since
-- the 'Subject's in the model are all initialized via 'addSubject'.
collectionDisplay :: (Functor t, Foldable t)
    => Window (t (Subject s)) ()
collectionDisplay = do
    ss <- view _model
    let toLi s = bh "li" [] (displaySubject s)
    bh "ul" [] (foldl' also alsoZero $ toLi <$> ss)

cleanupCollectionItem :: (Ord k)
    => k -> MaybeT (SceneState (M.Map k (Subject s))) ()
cleanupCollectionItem k = do
    old <- MaybeT $ use (_model.at k)
    cleanupSubject old

deleteCollectionItem :: (Ord k)
    => k -> MaybeT (SceneState (M.Map k (Subject s))) ()
deleteCollectionItem k = do
    cleanupCollectionItem k
    _model.at k .= Nothing

insertCollectionItem :: (Ord k)
    => k -> Subject s -> SceneState (M.Map k (Subject s)) ()
insertCollectionItem k sbj = do
    void $ runMaybeT $ cleanupCollectionItem k
    _model.at k .= Just sbj
