{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Initializer where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Extras as TE
import Data.Diverse.Profunctor
import qualified Glazier.React.Framework.Core.Model as R
import qualified Glazier.React.Framework.Core.Obj as R

-- | handledBy (installs event listeners, etc) a widget
-- where the event are completely handled.

-- | handledBy something that fires an event @c@
type Initializer m s c = s -> ContT () m c
type ObjInitializer m v s c = Initializer m (R.Obj v s) c
type SceneInitializer m v s c = Initializer m (R.Scene m v s) c

-- -- The identity for 'andInitializer''
-- nulInitializer' :: Initializer m r ()
-- nulInitializer' _ = pure ()

-- -- Activate left after the right.
-- -- The binary associative function for 'nulInitializer''.
-- andInitializer' :: Initializer m r () -> Initializer m r () -> Initializer m r ()
-- andInitializer' x y s = x s *> y s
-- infixr 6 `andInitializer'` -- like mappend

memptyInitializer :: forall c m s. Applicative m => Initializer m s c
memptyInitializer _ =  ContT $ const $ pure ()

mappendInitializer :: Applicative m => Initializer m s c -> Initializer m s c -> Initializer m s c
infixr 6 `mappendInitializer` -- like mappend
mappendInitializer f g s = f s `TE.seqContT` g s

-- The identity for 'andInitializer'
nulInitializer :: Applicative m => Initializer m r (Which '[])
nulInitializer =  memptyInitializer @(Which '[])

-- Activate left after the right.
-- The binary associative function for 'nulInitializer'.
andInitializer ::
    ( Applicative m
    , ChooseBoth c1 c2 c3
    ) =>
    Initializer m r (Which c1)
    -> Initializer m r (Which c2)
    -> Initializer m r (Which c3)
infixr 6 `andInitializer` -- like mappend
andInitializer x y s = (diversify <$> x s) `TE.seqContT` (diversify <$> y s)
