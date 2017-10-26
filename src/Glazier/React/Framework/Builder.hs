{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Builder where

import Control.Lens
import Control.Monad
import Data.Biapplicative
import Data.Diverse.Lens
import Data.IORef
import Data.Proxy
-- import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F

------------------------------------------------

newtype MkModel m p s = MkModel {
            runMkModel :: p -> m s -- make inactive model
            } deriving Functor

newtype MkModelPlanner m s p = MkModelPlanner { runMkModelPlanner :: MkModel m p s }

instance Contravariant (MkModelPlanner m s) where
    contramap f (MkModelPlanner (MkModel mkMdl)) = MkModelPlanner . MkModel $ mkMdl . f

instance F.IsPlanner (MkModel m p s) (MkModelPlanner m s) p where
    toPlanner = MkModelPlanner
    fromPlanner = runMkModelPlanner

instance R.MonadReactor m => F.ViaPlan (MkModelPlanner m s) where
    viaPlan l = contramap (view l)

------------------------------------------------

newtype MkPlan m s p = MkPlan {
            runMkPlan :: s -> m p
            } deriving Functor

newtype MkPlanModeller m p s = MkPlanModeller { runMkPlanModeller :: MkPlan m s p }

instance Contravariant (MkPlanModeller m s) where
    contramap f (MkPlanModeller (MkPlan mkPln)) = MkPlanModeller . MkPlan $ mkPln . f

instance F.IsModeller (MkPlan m s p) (MkPlanModeller m p) s where
    toModeller = MkPlanModeller
    fromModeller = runMkPlanModeller

instance R.MonadReactor m => F.ViaModel (MkPlanModeller m p) where
    viaModel l = contramap (view l)

------------------------------------------------

-- | @p' s'@ are the types the builder knows how to make
-- @p s@ are the type the builder knows how to read
newtype Builder m p s p' s' =
    Builder ( MkPlan m s p' -- from specifications
            , MkModel m p s' -- make inactive specifications
            )

newtype BuilderPlanner m s p' s' p = BuilderPlanner { runBuilderPlanner :: Builder m p s p' s' }

instance F.IsPlanner (Builder m p s p' s') (BuilderPlanner m s p' s') p where
    toPlanner = BuilderPlanner
    fromPlanner = runBuilderPlanner

instance R.MonadReactor m => F.ViaPlan (BuilderPlanner m s p' s') where
    viaPlan l (BuilderPlanner (Builder (mkPln, mkMdl))) =
        BuilderPlanner $ Builder (mkPln, F.viaPlan' l mkMdl)

newtype BuilderModeller m p p' s' s = BuilderModeller { runBuilderModeller :: Builder m p s p' s' }

instance F.IsModeller (Builder m p s p' s') (BuilderModeller m p p' s') s where
    toModeller = BuilderModeller
    fromModeller = runBuilderModeller

instance R.MonadReactor m => F.ViaModel (BuilderModeller m p p' s') where
    viaModel l (BuilderModeller (Builder (mkPln, mkMdl))) =
        BuilderModeller $ Builder (F.viaModel' l mkPln, mkMdl)

------------------------------------------------

instance Functor m => Bifunctor (Builder m p s) where
    bimap pq st (Builder (mkPlan, mkMdl)) = Builder (pq <$> mkPlan, st <$> mkMdl)

instance Applicative m => Biapplicative (Builder m p s) where
    bipure p s = Builder (MkPlan . const $ pure p, MkModel . const $ pure s)
    (Builder (MkPlan fMkPlan, MkModel fMkMdl)) <<*>> (Builder (MkPlan mkPlan, MkModel mkMdl)) =
        Builder ( MkPlan $ \p -> fMkPlan p <*> mkPlan p
                , MkModel $ \s -> fMkMdl s <*> mkMdl s
                )

instance R.MonadReactor m => F.IORefModel (Builder m p s p' s') (Builder m p s p' (IORef s')) where
    ioRefModel (Builder (mkPlan, MkModel mkModel)) = Builder (mkPlan, MkModel (mkModel >=> R.doNewIORef))

-- instance Applicative m => Semigroup (Builder m p s (Many '[]) (Many '[])) where
--     _ <> _ = nilBuilder

-- instance Applicative m => Monoid (Builder m p s (Many '[]) (Many '[])) where
--     mempty = nilBuilder
--     mappend = (<>)

-- | identity for 'andBuild'
nilBuilder :: Applicative m => Builder m p s (Many '[]) (Many '[])
nilBuilder = bipure nil nil

-- | Combine 'Builder's using 'Many'
andBuilder
    :: Applicative m
    => Builder m p s (Many p1) (Many s1)
    -> Builder m p s (Many p2) (Many s2)
    -> Builder m p s (Many (Append p1 p2)) (Many (Append s1 s2))
andBuilder (Builder (MkPlan mkPln, MkModel mkMdl)) (Builder (MkPlan mkPln', MkModel mkMdl')) =
    Builder ( MkPlan $ \s -> (/./) <$> mkPln s <*> mkPln' s
            , MkModel $ \p -> (/./) <$> mkMdl p <*> mkMdl' p
            )

-- | Add a type @x@ into the factory
build
    :: (Applicative m, UniqueMember x p, UniqueMember x s)
    => Proxy x -> Builder m (Many p) (Many s) (Many '[x]) (Many '[x])
build _ = Builder ( MkPlan $ pure . single . fetch
                  , MkModel $ pure . single . fetch
                  )
