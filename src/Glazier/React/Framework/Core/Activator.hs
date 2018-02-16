{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Activator where

import Control.Applicative
import Data.Diverse.Profunctor
import Data.Profunctor
import Data.Semigroup
import qualified Glazier.React.Framework.Core.Gate as F
import qualified Glazier.React.Framework.Core.Handler as F
import qualified Glazier.React.Framework.Core.Topic as F

------------------------------------------

type Activator m r b = F.Handler m r () b

type ObjActivator m v s b = F.ObjHandler m v s () b
type ProtoActivator m v s b = F.ProtoHandler m v s () b

toActivator :: (r -> (b -> x) -> x) -> F.Topic r (F.Gate x) () b
toActivator f = F.Topic ((\g -> F.Gate $ \k _ -> g k) <$> f)

fromActivator :: F.Topic r (F.Gate x)  ()b -> r -> (b -> x) -> x
fromActivator (F.Topic f) = (\(F.Gate g) -> \k -> g k ()) <$> f

-- | A friendlier constraint synonym for 'Executor' 'pmappend'.
type PmappendOutput b1 b2 b3 =
    ( Diversify b1 b3
    , Diversify b2 b3
    , b3 ~ AppendUnique b1 b2 -- ^ Redundant constraint: but narrows down @b3@
    )

nulActivator :: Applicative m => Activator m r (Which '[])
nulActivator = F.Topic . const $ F.pinned (pure ())

andActivator ::
    ( Applicative m
    , PmappendOutput b1 b2 b3
    )
    => Activator m r (Which b1)
    -> Activator m r (Which b2)
    -> Activator m r (Which b3)
andActivator (F.Topic x) (F.Topic y) = F.Topic $ \r -> F.meld (liftA2 (<>))
    (rmap diversify (x r))
    (rmap diversify (y r))
infixr 6 `andActivator` -- like mappend
