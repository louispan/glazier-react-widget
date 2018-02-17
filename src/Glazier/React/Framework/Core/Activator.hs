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

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Extras as E
import Data.Diverse.Profunctor
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F

-- | Activates (installs event listeners, etc) a widget
-- where the event are completely handled.
type Activator' m s = s -> m ()

-- | Activates something that fires an event @b@
type Activator m s b = s -> ContT () m b

type ObjActivator' m v s = Activator' m (F.Obj v s)
type ObjActivator m v s b = Activator m (F.Obj v s) b

type SceneActivator' m v s = Activator' m (F.Scene m v s)
type SceneActivator m v s b = Activator m (F.Scene m v s) b

nulActivator' :: Applicative m => Activator' m r
nulActivator' _ = pure ()

andActivator' :: Applicative m => Activator' m r -> Activator' m r -> Activator' m r
andActivator' x y s = x s *> y s

nulActivator :: Applicative m => Activator m r (Which '[])
nulActivator _ =  ContT $ \_ -> pure ()

-- run left after the right.
andActivator ::
    ( Applicative m
    , ChooseBoth b1 b2 b3
    )
    => Activator m r (Which b1)
    -> Activator m r (Which b2)
    -> Activator m r (Which b3)
andActivator x y s = (diversify <$> x s) `E.seqContT` (diversify <$> y s)
infixr 6 `andActivator` -- like mappend
