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

import qualified Glazier.Core as Z
import qualified Glazier.React.Framework.Core.Model as Z

type Initializer s m c = Z.Delegate s m c
type ObjInitializer v s m c = Initializer (Z.Obj IORef v s) m c
type SceneInitializer v s m c = Initializer (Z.Scene v s m) m c

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
