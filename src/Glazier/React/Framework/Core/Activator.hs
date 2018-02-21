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
import Control.Monad.Trans.Cont.Extras as TE
import Data.Diverse.Profunctor
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F

-- | Activates (installs event listeners, etc) a widget
-- where the event are completely handled.

-- | Activates something that fires an event @c@
type Activator m s c = s -> ContT () m c
type ObjActivator m v s c = Activator m (F.Obj v s) c
type SceneActivator m v s c = Activator m (F.Scene m v s) c

-- The identity for 'andActivator''
nulActivator' :: Activator m r ()
nulActivator' _ = pure ()

-- Activate left after the right.
-- The binary associative function for 'nulActivator''.
andActivator' :: Activator m r () -> Activator m r () -> Activator m r ()
andActivator' x y s = x s *> y s
infixr 6 `andActivator'` -- like mappend

-- The identity for 'andActivator'
nulActivator :: Applicative m => Activator m r (Which '[])
nulActivator _ =  ContT $ \_ -> pure ()

-- Activate left after the right.
-- The binary associative function for 'nulActivator'.
andActivator ::
    ( Applicative m
    , ChooseBoth b1 b2 b3
    )
    => Activator m r (Which b1)
    -> Activator m r (Which b2)
    -> Activator m r (Which b3)
andActivator x y s = (diversify <$> x s) `TE.seqContT` (diversify <$> y s)
infixr 6 `andActivator` -- like mappend
