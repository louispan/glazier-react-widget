{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.Rerender where

import Control.Lens
import Data.Generics.Product
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
import qualified JavaScript.Extras as JE

rerender :: R.MonadReactor x m => F.Scene x m v s -> m ()
rerender (F.Obj ref its) = do
    obj <- R.doReadIORef ref
    let (i, obj') = obj & (its.F.plan.field @"frameNum") <%~ ((+ 1) . (`mod` JE.maxSafeInteger))
    R.doWriteIORef ref obj'
    R.doSetComponentState
        (JE.fromProperties [("frameNum", JE.toJS' i)])
        (obj ^. (its.F.plan.field @"component"))
