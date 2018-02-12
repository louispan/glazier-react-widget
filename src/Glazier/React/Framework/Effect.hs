{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect where

import Control.Lens
import Data.Diverse.Lens
import qualified Data.DList as DL
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

-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
focus :: forall t x m v s.
    ( R.MonadReactor x m
    , HasItemTag' t R.EventTarget s)
    => F.Scene x m v s -> m (DL.DList x)
focus (F.Obj ref its) = do
    obj <- R.doReadIORef ref
    R.doFocus $ obj ^. its.F.model.itemTag' @t @R.EventTarget
    pure mempty
