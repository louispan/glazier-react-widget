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

import Control.Lens
import Data.Kind
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R

----------------------------------------------------------

type family Modeller (w :: Type -> Type) (s :: Type) = (r :: Type) | r -> w s

-- | Something that knows how to get and set (but not make) a model
class ViaModel (w :: Type -> Type) where
    -- | given a lens from @t@ to @s@,
    -- change something that knows how to manipulate an @s@
    -- to something that knows how to manipulate a @t@.
    viaModel :: Lens' t s -> Modeller w s -> Modeller w t

type family Planner (w :: Type -> Type) (p :: Type) = (r :: Type) | r -> w p

-- | Something that knows how to get and set (but not make) a plan
class ViaPlan (w :: Type -> Type) where
    -- | given lens from @q@ to @p@,
    -- change something that knows how to manipulate an @p@
    -- to something that knows how to manipulate a @q@.
    viaPlan :: Lens' q p -> Planner w p -> Planner w q

data ComponentModel = ComponentModel
    { component :: R.ReactComponent
    , componentDisposable :: R.Disposable () -- List of things to dispose on updated
    , componentKey :: R.ReactKey
    , componentUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , componentRender :: Maybe (J.Callback (IO J.JSVal))
    , componentFrameNum :: Int
    } deriving (G.Generic)

instance R.Dispose ComponentModel
