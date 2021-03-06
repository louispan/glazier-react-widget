{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.UKey
    ( UKey
    , zeroUKey
    , smallerUKey
    , largerUKey
    , betweenUKey
    ) where

import qualified GHC.Generics as G
import qualified JavaScript.Extras as JE
import qualified Data.Aeson as A

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

instance A.ToJSON UKey where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON UKey

instance A.ToJSONKey UKey

instance A.FromJSONKey UKey

zeroUKey :: UKey
zeroUKey = UKey []

ukeyStep :: Int
ukeyStep = 32

-- | Create a key smaller than the input key.
smallerUKey :: UKey -> UKey
smallerUKey (UKey []) = UKey [-ukeyStep]
smallerUKey (UKey (a : as)) = UKey $ case compare a (JE.minSafeInteger + ukeyStep) of
        LT -> if JE.minSafeInteger == a
            then JE.minSafeInteger : unUKey (smallerUKey (UKey as))
            else [JE.minSafeInteger]
        _ -> [a - ukeyStep]

-- | Create a key larger than the input key.
largerUKey :: UKey -> UKey
largerUKey (UKey []) = UKey [ukeyStep]
largerUKey (UKey (a : as)) = UKey $ case compare a (JE.maxSafeInteger - ukeyStep) of
        GT -> if JE.maxSafeInteger == a
            then JE.maxSafeInteger : unUKey (largerUKey (UKey as))
            else [JE.maxSafeInteger]
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
        then x : unUKey (betweenUKey (UKey xs) (UKey $ repeat JE.maxSafeInteger))
        else [betweenUncInt x y]
    GT -> if y + 1 == x
        then y : unUKey (betweenUKey (UKey ys) (UKey $ repeat JE.maxSafeInteger))
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
