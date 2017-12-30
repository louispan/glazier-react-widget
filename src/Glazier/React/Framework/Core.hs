{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core where

import qualified Control.Disposable as CD
import Control.Lens
import qualified Data.DList as DL
import Data.Kind
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R

----------------------------------------------------------

type family OnModel (w :: Type -> Type) (s :: Type) = (r :: Type) | r -> w s

-- | Something that knows how to get and set (but not make) a model
class ViaModel (w :: Type -> Type) where
    -- | given a lens from @t@ to @s@,
    -- change something that knows how to manipulate an @s@
    -- to something that knows how to manipulate a @t@.
    viaModel :: Lens' t s -> OnModel w s -> OnModel w t

type family OnInfo (w :: Type -> Type) (i :: Type) = (r :: Type) | r -> w i

-- | Something that knows how to get and set (but not make) a plan
class ViaInfo (w :: Type -> Type) where
    -- | given lens from @q@ to @p@,
    -- change something that knows how to manipulate an @p@
    -- to something that knows how to manipulate a @q@.
    viaInfo :: Lens' j i -> OnInfo w i -> OnInfo w j

data ComponentPlan x m = ComponentPlan
    { component :: R.ReactComponent
    , key :: R.ReactKey
    , frameNum :: Int
    , finalizer :: CD.Disposable -- things to dispose when this widget is removed
    , disposeOnUpdated :: CD.Disposable -- things to dispose on updated
    , doOnUpdated :: m (DL.DList x) -- additional monadic action to take after a rerender
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    } deriving (G.Generic)
