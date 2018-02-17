{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.Focus where

import Control.Lens
import Data.Diverse.Lens
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F

newtype Focus = Focus R.EventTarget

-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
focusRef :: forall t m v s.
    ( R.MonadReactor m
    , HasItemTag' t R.EventTarget s)
    => F.Scene m v s -> m Focus
focusRef (F.Obj ref its) = do
    obj <- R.doReadIORef ref
    pure . Focus $ obj ^. its.F.model.itemTag' @t @R.EventTarget
