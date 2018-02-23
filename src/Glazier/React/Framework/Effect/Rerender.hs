{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.Rerender where

import Control.Lens
import Data.Generics.Product
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as R
import qualified JavaScript.Extras as JE

rerender :: R.MonadReactor m => R.Scene m v s -> m () -> m ()
rerender (R.Obj ref its) k = do
    obj <- R.doReadIORef ref
    let (i, obj') = obj & its.R.plan.field @"afterOnUpdated" %~ (*> k)
                        & (its.R.plan.field @"frameNum") <%~ ((+ 1) . (`mod` JE.maxSafeInteger))
    R.doWriteIORef ref obj'
    R.doSetComponentState
        (JE.fromProperties [("frameNum", JE.toJS' i)])
        (obj ^. (its.R.plan.field @"component"))

rerender' :: R.MonadReactor m => R.Scene m v s -> m ()
rerender' this = rerender this (pure ())
