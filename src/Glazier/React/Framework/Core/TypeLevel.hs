{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.TypeLevel where

import Data.Diverse.TypeLevel
import Data.Kind
import Glazier.React.Framework.Core.TypeLevel.Internal

type family SameMembers (xs :: [Type]) (ys ::[Type]) :: Constraint where
    SameMembers xs ys = SameMembersImpl xs ys (Complement xs ys) (Complement ys xs)
