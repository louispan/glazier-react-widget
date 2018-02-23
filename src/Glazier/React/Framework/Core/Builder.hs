{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Builder where

import Control.Lens
import Data.Biapplicative
import Data.Coerce
import Data.Diverse.Lens
import Data.Tagged
import qualified Glazier.React.Framework.Core.Model as R

------------------------------------------------

-- | Make inactive model, monadic as it may need to create IORefs
newtype MkSpec m i s = MkSpec {
            unMkSpec :: i -> m s
            } deriving Functor

------------------------------------------------

newtype MkSpecOnInfo m s i = MkSpecOnInfo { unMkSpecOnInfo :: MkSpec m i s }

instance R.ViaInfo (MkSpecOnInfo m s) where
    type OnInfo (MkSpecOnInfo m s) i = MkSpec m i s
    viaInfo l (MkSpec mkSpc) = MkSpec $ mkSpc . view l

instance Contravariant (MkSpecOnInfo m s) where
    contramap f (MkSpecOnInfo (MkSpec g)) = coerce (g . f)

------------------------------------------------

-- | Monadic because we may need to 'R.doReadIORef' to get the data to make the info.
newtype MkInfo m s i = MkInfo {
            unMkInfo :: s -> m i
            } deriving Functor

------------------------------------------------

newtype MkInfoOnSpec m i s = MkInfoOnSpec { unMkInfoOnSpec :: MkInfo m s i }

instance R.ViaSpec (MkInfoOnSpec m i) where
    type OnSpec (MkInfoOnSpec m i) s = MkInfo m s i
    viaSpec l (MkInfo mkInf) = MkInfo $ mkInf . view l

instance Contravariant (MkInfoOnSpec m s) where
    contramap f (MkInfoOnSpec (MkInfo g)) = coerce (g . f)

------------------------------------------------

-- | @p' s'@ are the types the builder knows how to make
-- @p s@ are the type the builder knows how to read
newtype Builder m i s i' s' =
    Builder ( MkInfo m s i' -- from specifications
            , MkSpec m i s' -- make inactive specifications
            )

instance Functor m => Bifunctor (Builder m i s) where
    bimap ij st (Builder (mkInf, mkSpc)) = Builder (ij <$> mkInf, st <$> mkSpc)

instance Applicative m => Biapplicative (Builder m i s) where
    bipure i s = Builder (MkInfo . const $ pure i, MkSpec . const $ pure s)
    (Builder (MkInfo fMkInfo, MkSpec fMkMdl)) <<*>> (Builder (MkInfo mkInfo, MkSpec mkSpc)) =
        Builder ( MkInfo $ \i -> fMkInfo i <*> mkInfo i
                , MkSpec $ \s -> fMkMdl s <*> mkSpc s
                )

------------------------------------------------

newtype BuilderOnInfo m s i' s' i = BuilderOnInfo { unBuilderOnInfo :: Builder m i s i' s' }

instance R.ViaInfo (BuilderOnInfo m s i' s') where
    type OnInfo (BuilderOnInfo m s i' s') i = Builder m i s i' s'
    viaInfo l (Builder (mkInf, mkSpc)) =
        Builder (mkInf, R.viaInfo l mkSpc)

------------------------------------------------

newtype BuilderOnSpec m i i' s' s = BuilderOnSpec { unBuilderOnSpec :: Builder m i s i' s' }

instance R.ViaSpec (BuilderOnSpec m i i' s') where
    type OnSpec (BuilderOnSpec m i i' s') s = Builder m i s i' s'
    viaSpec l (Builder (mkInf, mkSpc)) =
        Builder (R.viaSpec l mkInf, mkSpc)

------------------------------------------------

-- | THe identity for 'andBuilder'
nulBuilder :: Applicative m => Builder m i s (Many '[]) (Many '[])
nulBuilder = bipure nil nil

-- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
type AndBuilder i1 i2 i3 s1 s2 s3 =
    ( i3 ~ Append i1 i2
    , s3 ~ Append s1 s2
    )

andBuilder ::
    (Applicative m
    , AndBuilder i1 i2 i3 s1 s2 s3
    ) =>
    Builder m i s (Many i1) (Many s1)
    -> Builder m i s (Many i2) (Many s2)
    -> Builder m i s (Many i3) (Many s3)
infixr 6 `andBuilder` -- like mappend
(Builder (MkInfo mkInf, MkSpec mkSpc)) `andBuilder`
    (Builder (MkInfo mkInf', MkSpec mkSpc')) =
        Builder
            ( MkInfo $ \s -> (/./) <$> mkInf s <*> mkInf' s
            , MkSpec $ \i -> (/./) <$> mkSpc i <*> mkSpc' i)

-- | A type restricted verison of const
-- where the right builder is a 'nulBuilder'.
-- It is useful for double checking that we can throw away the 'nulBuilder'
constBuilder :: Builder m i s i' s' -> Builder m i s (Many '[]) (Many '[]) -> Builder m i s i' s'
constBuilder = const

------------------------------------------------

-- | Return a builder that builds an item inside a Many
toItemBuilder
    :: forall m i s i' s' is ss.
    ( Applicative m
    , HasItem' i is
    , HasItem' s ss
    )
    => Builder m i s i' s'
    -> Builder m is ss (Many '[i']) (Many '[s'])
toItemBuilder (Builder (MkInfo mkInf, MkSpec mkSpc)) =
    Builder (MkInfo mkInf', MkSpec mkSpc')
  where
    mkInf' ss = single <$> mkInf (view (item' @s) ss)
    mkSpc' is = single <$> mkSpc (view (item' @i) is)


-- | Add a type @x@ into the model that is used directly from the info.
-- @forall@ so that the type can be specified first
build
    :: forall x m. (Applicative m)
    => Builder m x x x x
build = Builder ( MkInfo pure
                , MkSpec pure
                )

-- | Add a type @x@ into the model that is used directly from the info
-- and return a builder that uses a Many.
-- @forall@ so that the type can be specified first
buildItem
    :: forall x m i s. (Applicative m, HasItem' x i, HasItem' x s)
    => Builder m i s (Many '[x]) (Many '[x])
buildItem = toItemBuilder build

-- | Add a value @x@ into the model that is not from the info.
-- @forall@ so that the type can be specified first
hardcodeItem
    :: forall x m i s. Applicative m
    => x -> Builder m i s (Many '[]) (Many '[x])
hardcodeItem x = Builder ( MkInfo . const $ pure nil
                  , MkSpec . const . pure $ single x
                  )

-- | Add a value @x@ into the model that is not from the info.
-- @forall@ so that the type can be specified first
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
hardcodeItemTag
    :: forall t x m i s. Applicative m
    => x -> Builder m i s (Many '[]) (Many '[Tagged t x])
hardcodeItemTag x = Builder ( MkInfo . const $ pure nil
                  , MkSpec . const . pure . single $ Tagged x
                  )

-- -- | More descriptive name for 'second' for Builder
-- mapModel :: Functor m => (s' -> t') -> Builder m i s i' s' -> Builder m i s i' t'
-- mapModel = second

-- -- | More descriptive name for 'first' for Builder
-- mapInfo :: Functor m => (i' -> j') -> Builder m i s i' s' -> Builder m i s j' s'
-- mapInfo = first

-- dimapInfo :: Functor m => (j -> i) -> (i' -> j') -> Builder m i s i' s' -> Builder m j s j' s'
-- dimapInfo ji ij (Builder (mkInf, mkSpc)) =
--     Builder ( ij <$> mkInf
--             , coerce (contramap ji (MkSpecOnInfo mkSpc)))

-- dimapModel :: Functor m => (t -> s) -> (s' -> t') -> Builder m i s i' s' -> Builder m i t i' t'
-- dimapModel ts st (Builder (mkInf, mkSpc)) =
--     Builder ( coerce (contramap ts (MkInfoOnSpec mkInf))
--             , st <$> mkSpc)

-- transformBuilder :: Functor m
--     => (j -> i) -> (i' -> j')
--     -> (t -> s) -> (s' -> t')
--     -> Builder m i s i' s' -> Builder m j t j' t'
-- transformBuilder ji ij ts st (Builder (mkInf, mkSpc)) =
--     Builder
--         ( coerce (contramap ts (MkInfoOnSpec (ij <$> mkInf)))
--         , coerce (contramap ji (MkSpecOnInfo (st <$> mkSpc))))

-- bicontramapBuilder ::
--     (j -> i)
--     -> (t -> s)
--     -> Builder m i s i' s' -> Builder m j t i' s'
-- bicontramapBuilder ji ts (Builder (mkInf, mkSpc)) =
--     Builder
--         ( coerce (contramap ts (MkInfoOnSpec mkInf))
--         , coerce (contramap ji (MkSpecOnInfo mkSpc)))

-- bimapBuilder :: Functor m
--     => (i' -> j')
--     -> (s' -> t')
--     -> Builder m i s i' s' -> Builder m i s j' t'
-- bimapBuilder ij st (Builder (mkInf, mkSpc)) =
--     Builder
--         ( coerce (MkInfoOnSpec (ij <$> mkInf))
--         , coerce (MkSpecOnInfo (st <$> mkSpc)))

-- taggedBuilder :: forall t m i s i' s'.
--     Functor m
--     => Builder m i s i' s' -> Builder m (Tagged t i) (Tagged t s) (Tagged t i') (Tagged t s')
-- taggedBuilder = dimapModel unTagged Tagged . dimapInfo unTagged Tagged

