{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import qualified Glazier.React.Framework.Core as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

------------------------------------------------

newtype MkModel m p s = MkModel {
            runMkModel :: p -> m s -- make inactive model
            } deriving Functor

------------------------------------------------

newtype MkModelPlanner m s p = MkModelPlanner { runMkModelPlanner :: MkModel m p s }

type instance F.Planner (MkModelPlanner m s) p = MkModel m p s

instance F.ViaPlan (MkModelPlanner m s) where
    viaPlan l (MkModel mkMdl) = MkModel $ mkMdl . view l

instance Contravariant (MkModelPlanner m s) where
    contramap f (MkModelPlanner (MkModel g)) = coerce (g . f)

------------------------------------------------

newtype MkPlan m s p = MkPlan {
            runMkPlan :: s -> m p
            } deriving Functor

------------------------------------------------

newtype MkPlanModeller m p s = MkPlanModeller { runMkPlanModeller :: MkPlan m s p }

type instance F.Modeller (MkPlanModeller m p) s = MkPlan m s p

instance F.ViaModel (MkPlanModeller m p) where
    viaModel l (MkPlan mkPln) = MkPlan $ mkPln . view l

instance Contravariant (MkPlanModeller m s) where
    contramap f (MkPlanModeller (MkPlan g)) = coerce (g . f)

------------------------------------------------

-- | @p' s'@ are the types the builder knows how to make
-- @p s@ are the type the builder knows how to read
newtype Builder m p s p' s' =
    Builder ( MkPlan m s p' -- from specifications
            , MkModel m p s' -- make inactive specifications
            )

instance Functor m => Bifunctor (Builder m p s) where
    bimap pq st (Builder (mkPlan, mkMdl)) = Builder (pq <$> mkPlan, st <$> mkMdl)

instance Applicative m => Biapplicative (Builder m p s) where
    bipure p s = Builder (MkPlan . const $ pure p, MkModel . const $ pure s)
    (Builder (MkPlan fMkPlan, MkModel fMkMdl)) <<*>> (Builder (MkPlan mkPlan, MkModel mkMdl)) =
        Builder ( MkPlan $ \p -> fMkPlan p <*> mkPlan p
                , MkModel $ \s -> fMkMdl s <*> mkMdl s
                )

-- instance R.MonadReactor m => F.IORefModel
--         (Builder m p s p' s')
--         (Builder m p (IORef s) p' (IORef s')) where
--     ioRefModel (Builder (MkPlan mkPlan, MkModel mkModel)) = Builder
--         ( MkPlan (R.doReadIORef >=> mkPlan)
--         , MkModel (mkModel >=> R.doNewIORef)
--         )

------------------------------------------------

newtype BuilderPlanner m s p' s' p = BuilderPlanner { runBuilderPlanner :: Builder m p s p' s' }

type instance F.Planner (BuilderPlanner m s p' s') p = Builder m p s p' s'

instance F.ViaPlan (BuilderPlanner m s p' s') where
    viaPlan l (Builder (mkPln, mkMdl)) =
        Builder (mkPln, F.viaPlan l mkMdl)

------------------------------------------------

newtype BuilderModeller m p p' s' s = BuilderModeller { runBuilderModeller :: Builder m p s p' s' }

type instance F.Modeller (BuilderModeller m p p' s') s = Builder m p s p' s'

instance F.ViaModel (BuilderModeller m p p' s') where
    viaModel l (Builder (mkPln, mkMdl)) =
        Builder (F.viaModel l mkPln, mkMdl)

------------------------------------------------
newtype PBuilder m p s ps' = PBuilder
    { runPBuilder :: Builder m p s (P.At0 ps') (P.At1 ps')
    }

type instance P.PNullary (PBuilder m p s) (p', s') = Builder m p s p' s'

-- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
instance Applicative m => P.PMEmpty (PBuilder m p s) (Many '[], Many '[]) where
    pmempty = bipure nil nil

-- | UndecidableInstances!
instance (Applicative m, p3 ~ Append p1 p2, s3 ~ Append s1 s2) =>
         P.PSemigroup (PBuilder m p s) (Many p1, Many s1) (Many p2, Many s2) (Many p3, Many s3) where
    (Builder (MkPlan mkPln, MkModel mkMdl)) `pmappend`
        (Builder (MkPlan mkPln', MkModel mkMdl')) =
            Builder
                ( MkPlan $ \s -> (/./) <$> mkPln s <*> mkPln' s
                , MkModel $ \p -> (/./) <$> mkMdl p <*> mkMdl' p)

------------------------------------------------

-- | Return a builder that builds an item inside a Many
itemizing
    :: forall m p s p' s' ps ss.
    ( Applicative m
    , UniqueMember p ps
    , UniqueMember s ss
    )
    => Builder m p s p' s'
    -> Builder m (Many ps) (Many ss) (Many '[p']) (Many '[s'])
itemizing (Builder (MkPlan mkPln, MkModel mkMdl)) =
    Builder (MkPlan mkPln', MkModel mkMdl')
  where
    mkPln' ss = single <$> mkPln (fetch @s ss)
    mkMdl' ps = single <$> mkMdl (fetch @p ps)


-- | Add a type @x@ into the model that is used directly from the plan.
-- @forall@ so that the type can be specified first
build'
    :: forall x m proxy. (Applicative m)
    => proxy x -> Builder m x x x x
build' _ = Builder ( MkPlan $ pure
                  , MkModel $ pure
                  )

-- FIXME: Only allow certain things for react components vs prototypes
-- | Add a type @x@ into the model that is used directly from the plan
-- and return a builder that uses a Many.
-- @forall@ so that the type can be specified first
build
    :: forall x m p s proxy. (Applicative m, UniqueMember x p, UniqueMember x s)
    => proxy x -> Builder m (Many p) (Many s) (Many '[x]) (Many '[x])
build = itemizing . build'

-- | Add a value @x@ into the model that is not from the plan.
-- @forall@ so that the type can be specified first
-- Intentional redunadant constraint of (UniqueMember x s)
hardcode
    :: forall x m p s. (Applicative m, UniqueMember x s)
    => x -> Builder m p (Many s) (Many '[]) (Many '[x])
hardcode x = Builder ( MkPlan . const $ pure nil
                  , MkModel . const . pure $ single x
                  )

dimapPlan :: Functor m => (q -> p) -> (p' -> q') -> Builder m p s p' s' -> Builder m q s q' s'
dimapPlan f g (Builder (mkPln, mkMdl)) =
    Builder ( g <$> mkPln
            , coerce (contramap f (MkModelPlanner mkMdl)))

dimapModel :: Functor m => (t -> s) -> (s' -> t') -> Builder m p s p' s' -> Builder m p t p' t'
dimapModel f g (Builder (mkPln, mkMdl)) =
    Builder ( coerce (contramap f (MkPlanModeller mkPln))
            , g <$> mkMdl)
