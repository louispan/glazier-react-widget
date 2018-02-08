{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Commands.Rerender where

import Control.Applicative
import Control.Lens
import Control.Lens.Misc
import Data.Diverse
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import Data.Semigroup
import qualified Glazier.React as R
import Glazier.React.Framework.Core as F
import qualified JavaScript.Extras as JE

data Rerender = Rerender R.ReactComponent [JE.Property]

mkRerender :: R.MonadReactor x m => IORef v -> Lens' v (ComponentPlan x m) -> m (DL.DList x) -> m Rerender
mkRerender ref this f = do
    obj <- R.doReadIORef ref
    let (i, obj') = obj & (this . field @"doOnUpdated") `over'` (\g -> liftA2 (<>) g f)
                        & (this . field @"frameNum") <%~ ((+ 1) . (`mod` JE.maxSafeInteger))
    R.doWriteIORef ref obj'
    pure $ Rerender (obj ^. (this . field @"component")) [("frameNum", JE.toJS' i)]

mkRerender' ::
    ( R.MonadReactor x m
    , UniqueMember Rerender a)
    => IORef v
    -> Lens' v (ComponentPlan x m, s)
    -> m (DL.DList (Which a))
mkRerender' ref this = (DL.singleton . pick) <$> mkRerender ref (this._1) (pure mempty)
