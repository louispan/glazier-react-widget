{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Builder where

import Control.Lens
-- import Control.Monad
-- import Control.Monad.Reader
import Data.Biapplicative
import Data.Diverse.Lens
-- import Data.Kind
import Data.Proxy
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F

------------------------------------------------

newtype MkModel p m s = MkModel {
            runMkModel :: p -> m s -- make inactive model
            } deriving Functor

newtype MkModel_Planner s m p = MkModel_Planner { runMkModel_Planner :: MkModel p m s }

instance Contravariant (MkModel_Planner s m) where
    contramap f (MkModel_Planner (MkModel mkMdl)) = MkModel_Planner . MkModel $ mkMdl . f

instance F.Planner (MkModel p m s) (MkModel_Planner s m) p where
    toPlanner = MkModel_Planner
    fromPlanner = runMkModel_Planner

instance R.MonadReactor m => F.ViaPlan (MkModel_Planner s m) where
    viaPlan l = contramap (view l)

------------------------------------------------

newtype MkPlan s m p = MkPlan {
            runMkPlan :: s -> m p
            } deriving Functor

newtype MkPlan_Modeller p m s = MkPlan_Modeller { runMkPlan_Modeller :: MkPlan s m p }

instance Contravariant (MkPlan_Modeller s m) where
    contramap f (MkPlan_Modeller (MkPlan mkPln)) = MkPlan_Modeller . MkPlan $ mkPln . f

instance F.Modeller (MkPlan s m p) (MkPlan_Modeller p m) s where
    toModeller = MkPlan_Modeller
    fromModeller = runMkPlan_Modeller

instance R.MonadReactor m => F.ViaModel (MkPlan_Modeller p m) where
    viaModel l = contramap (view l)

------------------------------------------------

-- | @p' s'@ are the types the builder knows how to make
-- @p s@ are the type the builder knows how to read
newtype Builder p s m p' s' =
    Builder ( MkPlan s m p' -- from specifications
            , MkModel p m s' -- make inactive specifications
            )

newtype Builder_Planner s p' s' m p = Builder_Planner { runBuilder_Planner :: Builder p s m p' s' }

instance F.Planner (Builder p s m p' s') (Builder_Planner s p' s' m) p where
    toPlanner = Builder_Planner
    fromPlanner = runBuilder_Planner

instance R.MonadReactor m => F.ViaPlan (Builder_Planner s p' s' m) where
    viaPlan l (Builder_Planner (Builder (mkPln, mkMdl))) =
        Builder_Planner $ Builder (mkPln, F.viaPlan' l mkMdl)

newtype Builder_Modeller p p' s' m s = Builder_Modeller { runBuilder_Modeller :: Builder p s m p' s' }

instance F.Modeller (Builder p s m p' s') (Builder_Modeller p p' s' m ) s where
    toModeller = Builder_Modeller
    fromModeller = runBuilder_Modeller

instance R.MonadReactor m => F.ViaModel (Builder_Modeller p p' s' m) where
    viaModel l (Builder_Modeller (Builder (mkPln, mkMdl))) =
        Builder_Modeller $ Builder (F.viaModel' l mkPln, mkMdl)

------------------------------------------------

instance Functor m => Bifunctor (Builder p s m) where
    bimap pq st (Builder (mkPlan, mkMdl)) = Builder (pq <$> mkPlan, st <$> mkMdl)

instance Applicative m => Biapplicative (Builder p s m) where
    bipure p s = Builder (MkPlan . const $ pure p, MkModel . const $ pure s)
    (Builder (MkPlan fMkPlan, MkModel fMkMdl)) <<*>> (Builder (MkPlan mkPlan, MkModel mkMdl)) =
        Builder ( MkPlan $ \p -> fMkPlan p <*> mkPlan p
                , MkModel $ \s -> fMkMdl s <*> mkMdl s
                )

-- | identity for 'andBuild'
nilBuilder :: Applicative m => Builder p s m (Many '[]) (Many '[])
nilBuilder = bipure nil nil

-- | Combine 'Builder's using 'Many'
andBuilder
    :: Applicative m
    => Builder p s m (Many p1) (Many s1)
    -> Builder p s m (Many p2) (Many s2)
    -> Builder p s m (Many (Append p1 p2)) (Many (Append s1 s2))
andBuilder (Builder (MkPlan mkPln, MkModel mkMdl)) (Builder (MkPlan mkPln', MkModel mkMdl')) =
    Builder ( MkPlan $ \s -> (/./) <$> mkPln s <*> mkPln' s
            , MkModel $ \p -> (/./) <$> mkMdl p <*> mkMdl' p
            )

-- | Add a type @x@ into the factory
build
    :: (Applicative m, UniqueMember x p, UniqueMember x s)
    => Proxy x -> Builder (Many p) (Many s) m (Many '[x]) (Many '[x])
build _ = Builder ( MkPlan $ pure . single . fetch
                  , MkModel $ pure . single . fetch
                  )
