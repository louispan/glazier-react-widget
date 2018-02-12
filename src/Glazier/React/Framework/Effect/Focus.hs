{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.Focus where

import Control.Lens
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F

newtype Focus = Focus R.EventTarget

-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
focusRef :: forall t x m v s.
    ( R.MonadReactor x m
    , HasItemTag' t R.EventTarget s)
    => F.Scene x m v s -> m (DL.DList (Which '[Focus]))
focusRef (F.Obj ref its) = do
    obj <- R.doReadIORef ref
    pure . DL.singleton . pickOnly . Focus $ obj ^. its.F.model.itemTag' @t @R.EventTarget
