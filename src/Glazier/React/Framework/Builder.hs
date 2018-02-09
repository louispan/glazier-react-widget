{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Builder where

import Control.Lens
import Data.Biapplicative
import Data.Coerce
import Data.Diverse.Lens
import Data.Tagged
import qualified Glazier.React.Framework.Model as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

------------------------------------------------

-- | Make inactive model, monadic as it may need to create IORefs
newtype MkModel m i s = MkModel {
            runMkModel :: i -> m s
            } deriving Functor

------------------------------------------------

newtype MkModelOnInfo m s i = MkModelOnInfo { runMkModelOnInfo :: MkModel m i s }

type instance F.OnInfo (MkModelOnInfo m s) i = MkModel m i s

instance F.ViaInfo (MkModelOnInfo m s) where
    viaInfo l (MkModel mkMdl) = MkModel $ mkMdl . view l

instance Contravariant (MkModelOnInfo m s) where
    contramap f (MkModelOnInfo (MkModel g)) = coerce (g . f)

------------------------------------------------

-- | Monadic because we may need to 'R.doReadIORef' to get the data to make the info.
newtype MkInfo m s i = MkInfo {
            runMkInfo :: s -> m i
            } deriving Functor

------------------------------------------------

newtype MkInfoOnModel m i s = MkInfoOnModel { runMkInfoOnModel :: MkInfo m s i }

type instance F.OnModel (MkInfoOnModel m i) s = MkInfo m s i

instance F.ViaModel (MkInfoOnModel m i) where
    viaModel l (MkInfo mkInf) = MkInfo $ mkInf . view l

instance Contravariant (MkInfoOnModel m s) where
    contramap f (MkInfoOnModel (MkInfo g)) = coerce (g . f)

------------------------------------------------

-- | @p' s'@ are the types the builder knows how to make
-- @p s@ are the type the builder knows how to read
newtype Builder m i s i' s' =
    Builder ( MkInfo m s i' -- from specifications
            , MkModel m i s' -- make inactive specifications
            )

instance Functor m => Bifunctor (Builder m i s) where
    bimap ij st (Builder (mkInfo, mkMdl)) = Builder (ij <$> mkInfo, st <$> mkMdl)

instance Applicative m => Biapplicative (Builder m i s) where
    bipure i s = Builder (MkInfo . const $ pure i, MkModel . const $ pure s)
    (Builder (MkInfo fMkInfo, MkModel fMkMdl)) <<*>> (Builder (MkInfo mkInfo, MkModel mkMdl)) =
        Builder ( MkInfo $ \i -> fMkInfo i <*> mkInfo i
                , MkModel $ \s -> fMkMdl s <*> mkMdl s
                )

------------------------------------------------

newtype BuilderOnInfo m s i' s' i = BuilderOnInfo { runBuilderOnInfo :: Builder m i s i' s' }

type instance F.OnInfo (BuilderOnInfo m s i' s') i = Builder m i s i' s'

instance F.ViaInfo (BuilderOnInfo m s i' s') where
    viaInfo l (Builder (mkInf, mkMdl)) =
        Builder (mkInf, F.viaInfo l mkMdl)

------------------------------------------------

newtype BuilderOnModel m i i' s' s = BuilderOnModel { runBuilderOnModel :: Builder m i s i' s' }

type instance F.OnModel (BuilderOnModel m i i' s') s = Builder m i s i' s'

instance F.ViaModel (BuilderOnModel m i i' s') where
    viaModel l (Builder (mkInf, mkMdl)) =
        Builder (F.viaModel l mkInf, mkMdl)

------------------------------------------------
newtype PBuilder m i s is' = PBuilder
    { runPBuilder :: Builder m i s (P.At0 is') (P.At1 is')
    }

-- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
type PmappendBuilder i1 i2 i3 s1 s2 s3 =
    ( i3 ~ Append i1 i2
    , s3 ~ Append s1 s2
    )

type instance P.PNullary (PBuilder m i s) (i', s') = Builder m i s i' s'

-- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
instance Applicative m => P.PMEmpty (PBuilder m i s) (Many '[], Many '[]) where
    pmempty = bipure nil nil

-- | UndecidableInstances!
instance (Applicative m
         , PmappendBuilder i1 i2 i3 s1 s2 s3
         ) =>
         P.PSemigroup (PBuilder m i s) (Many i1, Many s1) (Many i2, Many s2) (Many i3, Many s3) where
    (Builder (MkInfo mkInf, MkModel mkMdl)) `pmappend`
        (Builder (MkInfo mkInf', MkModel mkMdl')) =
            Builder
                ( MkInfo $ \s -> (/./) <$> mkInf s <*> mkInf' s
                , MkModel $ \i -> (/./) <$> mkMdl i <*> mkMdl' i)

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
toItemBuilder (Builder (MkInfo mkInf, MkModel mkMdl)) =
    Builder (MkInfo mkInf', MkModel mkMdl')
  where
    mkInf' ss = single <$> mkInf (view (item' @s) ss)
    mkMdl' is = single <$> mkMdl (view (item' @i) is)


-- | Add a type @x@ into the model that is used directly from the info.
-- @forall@ so that the type can be specified first
build
    :: forall x m. (Applicative m)
    => Builder m x x x x
build = Builder ( MkInfo pure
                , MkModel pure
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
                  , MkModel . const . pure $ single x
                  )

-- | Add a value @x@ into the model that is not from the info.
-- @forall@ so that the type can be specified first
-- Using @AllowAmbiguousTypes@ instead of @Proxy@
hardcodeItemTag
    :: forall t x m i s. Applicative m
    => x -> Builder m i s (Many '[]) (Many '[Tagged t x])
hardcodeItemTag x = Builder ( MkInfo . const $ pure nil
                  , MkModel . const . pure . single $ Tagged x
                  )

-- -- | More descriptive name for 'second' for Builder
-- mapModel :: Functor m => (s' -> t') -> Builder m i s i' s' -> Builder m i s i' t'
-- mapModel = second

-- -- | More descriptive name for 'first' for Builder
-- mapInfo :: Functor m => (i' -> j') -> Builder m i s i' s' -> Builder m i s j' s'
-- mapInfo = first

-- dimapInfo :: Functor m => (j -> i) -> (i' -> j') -> Builder m i s i' s' -> Builder m j s j' s'
-- dimapInfo ji ij (Builder (mkInf, mkMdl)) =
--     Builder ( ij <$> mkInf
--             , coerce (contramap ji (MkModelOnInfo mkMdl)))

-- dimapModel :: Functor m => (t -> s) -> (s' -> t') -> Builder m i s i' s' -> Builder m i t i' t'
-- dimapModel ts st (Builder (mkInf, mkMdl)) =
--     Builder ( coerce (contramap ts (MkInfoOnModel mkInf))
--             , st <$> mkMdl)

mapBuilder :: Functor m
    => (j -> i) -> (i' -> j')
    -> (t -> s) -> (s' -> t')
    -> Builder m i s i' s' -> Builder m j t j' t'
mapBuilder ji ij ts st (Builder (mkInf, mkMdl)) =
    Builder
        ( coerce (contramap ts (MkInfoOnModel (ij <$> mkInf)))
        , coerce (contramap ji (MkModelOnInfo (st <$> mkMdl))))
-- taggedBuilder :: forall t m i s i' s'.
--     Functor m
--     => Builder m i s i' s' -> Builder m (Tagged t i) (Tagged t s) (Tagged t i') (Tagged t s')
-- taggedBuilder = dimapModel unTagged Tagged . dimapInfo unTagged Tagged


-- wack :: Functor m => Iso' t s -> Builder m i s i' s -> Builder m i t i' t
-- wack l = dimapModel (view l) (review l)

-- wock :: Iso' t s -> Lens' t s
-- wock a = a
