{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Builder where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F

newtype MkModel p m s = MkModel {
            runMkModel :: p -> m s -- make inactive model
            } deriving Functor

instance F.AModelWrapper (MkModel p m s) (MkModel p) m s where
    toModelWrapper = id
    fromModelWrapper = id

instance R.MonadReactor m => F.ModelWrapper (MkModel p) m where
    wrapModel f _ mkMdl = f <$> mkMdl
    wrapMModel f _ (MkModel mkMdl) = MkModel (mkMdl >=> f)

newtype MkModel_PlanWrapper s m p = MkModel_PlanWrapper { runMkModel_PlanWrapper :: MkModel p m s }

instance F.APlanWrapper (MkModel p m s) (MkModel_PlanWrapper s) m p where
    toPlanWrapper = MkModel_PlanWrapper
    fromPlanWrapper (MkModel_PlanWrapper a) = a

instance R.MonadReactor m => F.PlanWrapper (MkModel_PlanWrapper s) m where
    wrapPlan _ g (MkModel_PlanWrapper (MkModel mkMdl)) =
        MkModel_PlanWrapper . MkModel $ mkMdl . g
    wrapMPlan _ g (MkModel_PlanWrapper (MkModel mkMdl)) =
        MkModel_PlanWrapper . MkModel $ g >=> mkMdl

newtype MkPlan s m p = MkPlan {
            runMkPlan :: s -> m p -- make inactive model
            } deriving Functor

instance F.APlanWrapper (MkPlan s m p) (MkPlan s) m p where
    toPlanWrapper = id
    fromPlanWrapper = id

instance R.MonadReactor m => F.PlanWrapper (MkPlan s) m where
    wrapPlan f _ mkMdl = f <$> mkMdl
    wrapMPlan f _ (MkPlan mkPln) = MkPlan (mkPln >=> f)

newtype MkPlan_ModelWrapper p m s = MkPlan_ModelWrapper { runMkPlan_ModelWrapper :: MkPlan s m p }

instance F.AModelWrapper (MkPlan s m p) (MkPlan_ModelWrapper p) m s where
    toModelWrapper = MkPlan_ModelWrapper
    fromModelWrapper (MkPlan_ModelWrapper a) = a

instance R.MonadReactor m => F.ModelWrapper (MkPlan_ModelWrapper p) m where
    wrapModel _ g (MkPlan_ModelWrapper (MkPlan mkMdl)) =
        MkPlan_ModelWrapper . MkPlan $ mkMdl . g
    wrapMModel _ g (MkPlan_ModelWrapper (MkPlan mkMdl)) =
        MkPlan_ModelWrapper . MkPlan $ g >=> mkMdl

newtype Builder m p' p s' s =
    Builder ( MkPlan s m p' -- from specifications
            , MkModel p m s' -- make inactive specifications
            )

newtype Builder_PlanWrapper s' s m p = Builder_PlanWrapper { runBuilder_PlanWrapper :: Builder m p p s' s }

instance F.APlanWrapper (Builder m p p s' s) (Builder_PlanWrapper s' s) m p where
    toPlanWrapper = Builder_PlanWrapper
    fromPlanWrapper (Builder_PlanWrapper a) = a

instance R.MonadReactor m => F.PlanWrapper (Builder_PlanWrapper s' s) m where
    wrapPlan f g (Builder_PlanWrapper (Builder (mkPln, mkMdl))) =
        Builder_PlanWrapper $ Builder (F.wrapPlan' f g mkPln, F.wrapPlan' f g mkMdl)
    wrapMPlan f g (Builder_PlanWrapper (Builder (mkPln, mkMdl))) =
        Builder_PlanWrapper $ Builder (F.wrapMPlan' f g mkPln, F.wrapMPlan' f g mkMdl)


newtype Builder_ModelWrapper p' p m s = Builder_ModelWrapper { runBuilder_ModelWrapper :: Builder m p' p s s }

instance F.AModelWrapper (Builder m p' p s s) (Builder_ModelWrapper p' p) m s where
    toModelWrapper = Builder_ModelWrapper
    fromModelWrapper (Builder_ModelWrapper a) = a

instance R.MonadReactor m => F.ModelWrapper (Builder_ModelWrapper p' p) m where
    wrapModel f g (Builder_ModelWrapper (Builder (mkPln, mkMdl))) =
        Builder_ModelWrapper $ Builder (F.wrapModel' f g mkPln, F.wrapModel' f g mkMdl)
    wrapMModel f g (Builder_ModelWrapper (Builder (mkPln, mkMdl))) =
        Builder_ModelWrapper $ Builder (F.wrapMModel' f g mkPln, F.wrapMModel' f g mkMdl)



-- | identity for 'andBuild'
idle :: Applicative m => Builder m (Many '[]) p (Many '[]) s
idle = Builder (MkPlan . const $ pure nil, MkModel . const $ pure nil)

andBuilder
    :: Applicative m
    => Builder m (Many p1) p (Many s1) s
    -> Builder m (Many p2) p (Many s2) s
    -> Builder m (Many (Append p1 p2)) p (Many (Append s1 s2)) s
andBuilder (Builder (MkPlan mkPln, MkModel mkMdl)) (Builder (MkPlan mkPln', MkModel mkMdl')) =
    Builder ( MkPlan $ \s -> (/./) <$> mkPln s <*> mkPln' s
            , MkModel $ \p -> (/./) <$> mkMdl p <*> mkMdl' p
            )

-- | Add a type @x@ into the factory
build
    :: (Applicative m, UniqueMember x p, UniqueMember x s)
    => Proxy x -> Builder m (Many '[x]) (Many p) (Many '[x]) (Many s)
build _ = Builder ( MkPlan $ pure . single . fetch
                  , MkModel $ pure . single . fetch
                  )
