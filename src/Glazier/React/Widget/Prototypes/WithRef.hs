{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widget.Prototypes.WithRef where

import Control.Lens
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Tagged
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Event.Internal as R
import qualified Glazier.React.Framework as F
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P

-- | Use with 'widget' for the builder of @@DL.DList R.Listener@
-- This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an R.EventTarget
-- in the model
-- Using @AllowAmbiguousTypes@ instead of @Proxy@
withRef
    :: forall t x m v i s.
        ( R.MonadReactor x m
        , HasItemTag' t [R.Listener] s
        , HasItemTag' t R.EventTarget s
        )
    => F.Prototype x m v i s
            (Many '[])
            (Many '[Tagged t R.EventTarget])
            (Which '[])
            (Which '[])
            (Which '[])
            (Which '[])
withRef =
    F.Prototype
        (F.hardcodeItemTag @t (R.EventTarget $ JE.JSVar J.nullRef))
        mempty
        mempty
        (F.controlledTrigger' @t
          "ref"
          (pure . DL.singleton . R.EventTarget . JE.JSVar)
          (F.delegate (F.Handler whenRef)))
        P.pmempty
  where
    whenRef ::
      F.Scene x m v s
      -> R.EventTarget
      -> m (DL.DList (Which '[]))
    whenRef (F.Obj ref its) j = do
            R.doModifyIORef' ref (set' (its._2.itemTag' @t) j)
            pure mempty
