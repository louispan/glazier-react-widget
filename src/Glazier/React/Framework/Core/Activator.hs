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

import Data.Diverse.Profunctor
import Data.Profunctor
import Data.Semigroup
import qualified Data.Semigroup.Applicative as S
import qualified Glazier.React.Framework.Core.Gate as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F
import qualified Glazier.React.Framework.Core.Topic as F

type Activator' m r = r -> m ()
type ObjActivator' m v s = F.Obj v s -> m ()
type SceneActivator' m v s = F.Scene m v s -> m ()

type Activator m r b = r -> (b -> m ()) -> m ()
type ObjActivator m v s b = F.Obj v s -> (b -> m ()) -> m ()
type SceneActivator m v s b = F.Scene m v s -> (b -> m ()) -> m ()

toTopicActivator :: (r -> (b -> m ()) -> m ()) -> F.Topic r (F.Gate (S.Ap m ())) () b
toTopicActivator f = F.Topic ((\g -> F.Gate $ \k _ -> S.Ap (g (S.getAp . k))) <$> f)

fromTopicActivator :: F.Topic r (F.Gate (S.Ap m ())) () b -> r -> (b -> m ()) -> m ()
fromTopicActivator (F.Topic f) = (\(F.Gate g) -> \k -> S.getAp (g (S.Ap . k) ())) <$> f

nulActivator' :: Applicative m => Activator' m r
nulActivator' _ = pure ()

andActivator' :: Applicative m => Activator' m r -> Activator' m r -> Activator' m r
andActivator' x y r = x r *> y r

nulActivator :: Applicative m => Activator m r (Which '[])
nulActivator _ _ =  pure ()

andActivator ::
    ( Applicative m
    , ChooseBoth b1 b2 b3
    )
    => Activator m r (Which b1)
    -> Activator m r (Which b2)
    -> Activator m r (Which b3)
andActivator x y = fromTopicActivator $ (rmap diversify (toTopicActivator x)) <> (rmap diversify (toTopicActivator y))
infixr 6 `andActivator` -- like mappend
