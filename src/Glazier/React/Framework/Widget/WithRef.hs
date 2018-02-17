{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Widget.WithRef where

import Control.Lens
import Control.Monad.Trans.Class
import Data.Diverse.Lens
import Data.Tagged
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Event.Internal as R
import qualified Glazier.React.Framework.Core as F
import qualified JavaScript.Extras as JE

-- | Use with 'widget' for the builder of @@DL.DList R.Listener@
-- This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an R.EventTarget
-- in the model
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
withRef :: forall t m v i s.
    ( R.MonadReactor m
    , HasItemTag' t [R.Listener] s
    , HasItemTag' t R.EventTarget s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged t R.EventTarget])
        (Which '[])
        (Which '[])
        (Which '[])
withRef =
    F.Prototype
        (F.hardcodeItemTag @t (R.EventTarget $ JE.JSVar J.nullRef))
        mempty
        F.nulFinalizer
        (F.controlledTrigger @t
            "ref"
            (pure. R.EventTarget . JE.JSVar)
            hdlRef)
        F.nulHandler
  where
    hdlRef :: F.SceneHandler m v s (R.EventTarget) (Which '[])
    hdlRef (F.Obj ref its) j = F.terminate' . lift $
        R.doModifyIORef' ref (set' (its.F.model.itemTag' @t) j)
