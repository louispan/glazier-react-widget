{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.TypeLevel where

import Data.Kind
import Data.Diverse.TypeLevel
import Glazier.React.Framework.TypeLevel.Internal

type family SameMembers (xs :: [Type]) (ys ::[Type]) :: Constraint where
    SameMembers xs ys = SameMembersImpl xs ys (Complement xs ys) (Complement ys xs)
