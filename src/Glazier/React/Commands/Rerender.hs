-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}

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

module Glazier.React.Commands.Rerender where

import Control.Lens
import Data.IORef
import Data.Generics.Product
import qualified Glazier.React as R
import Glazier.React.Framework.Core
import qualified JavaScript.Extras as JE

data Rerender = Rerender R.ReactComponent [JE.Property]

mkRerender :: R.MonadReactor x m => IORef v -> Lens' v ComponentModel -> m Rerender
mkRerender ref this = do
    obj <- R.doReadIORef ref
    let (i, obj') = obj & (this . field @"componentFrameNum") <%~ ((+ 1) . (`mod` JE.maxSafeInteger))
    R.doWriteIORef ref obj'
    pure $ Rerender (obj ^. (this . field @"component")) [("frameNum", JE.toJS' i)]
