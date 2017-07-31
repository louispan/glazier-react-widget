{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.TypeLevel.Internal where

import Data.Kind
import GHC.TypeLits

type family SameMembersImpl (xs :: [Type]) (ys ::[Type]) (xs' :: [Type]) (ys' ::[Type]) :: Constraint where
    SameMembersImpl xs ys '[] '[] = ()
    SameMembersImpl xs ys xs' ys' = TypeError ('Text "IndexOf error: ‘"
                                               ':<>: 'ShowType xs
                                               ':<>: 'Text "’"
                                               ':<>: 'Text " does not have same members as "
                                               ':<>: 'Text "‘"
                                               ':<>: 'ShowType ys
                                               ':<>: 'Text "’")
