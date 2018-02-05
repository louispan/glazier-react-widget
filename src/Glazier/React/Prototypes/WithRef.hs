{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Prototypes.WithRef where

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
    :: forall t m v i s x.
        ( R.MonadReactor x m
        , HasItemTag' t [R.Listener] s
        , HasItemTag' t R.EventTarget s
        )
    => F.Prototype m v i s
            (Many '[])
            (Many '[Tagged t R.EventTarget])
            x
            (Which '[])
            (Which '[])
            (Which '[])
            (Which '[])
withRef =
    F.Prototype
        (F.hardcodeItemTag @t (R.EventTarget $ JE.JSVar J.nullRef))
        mempty
        mempty
        (F.simpleExecutor (lmap obvious hdl) `F.handlesExActivator`
          F.triggerExObjActivator @t [F.Trigger ("ref", pure . DL.singleton . pickOnly . R.EventTarget . JE.JSVar)])
        P.pmempty
  where
    hdl :: F.ObjHandler m v (F.ComponentPlan x m, s) R.EventTarget (Which '[])
    hdl = F.viaModel (alongside id (itemTag' @t)) (F.Handler whenSetRef)

    whenSetRef ::
      F.Object v (F.ComponentPlan x m, R.EventTarget)
      -> R.EventTarget
      -> m (DL.DList (Which '[]))
    whenSetRef (F.Object ref (Lens this)) j = do
            R.doModifyIORef' ref (set' (this._2) j)
            pure mempty
