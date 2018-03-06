-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Initializer where

-- import Control.Monad.Trans.Cont
-- import Control.Monad.Trans.Cont.Extras as TE
-- import Data.Diverse.Profunctor
-- import Data.Semigroup
import Data.IORef

import Glazier.Core
import Glazier.React.Framework.Core.Model

type Initializer s m c = Delegate s m c
type ObjInitializer p s m c = Initializer (Obj IORef p s) m c
type SceneInitializer p s m c = Initializer (Scene p m s) m c

-- -- mempty
-- memptyInitializer :: Applicative m => Initializer s m c
-- memptyInitializer = mempty

-- -- mappend
-- mappendInitializer :: Applicative m => Initializer s m c -> Initializer s m c -> Initializer s m c
-- infixr 6 `mappendInitializer` -- like mappend
-- mappendInitializer = mappend

-- -- Activate left after the right, firing only the result from the right.
-- -- The binary associative function for 'nulInitializer'.
-- thenInitializer :: Initializer m r () -> Initializer m r a -> Initializer m r a
-- thenInitializer = (*>)

-- -- The identity for 'alsoInitializer'
-- nulInitializer :: Applicative m => Initializer m s (Which '[])
-- nulInitializer =  memptyInitializer @(Which '[])
