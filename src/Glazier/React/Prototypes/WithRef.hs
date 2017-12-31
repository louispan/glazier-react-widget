{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Prototypes.WithRef where

import Control.Lens
import Control.DeepSeq
import Data.Diverse.Lens
import Data.IORef
import qualified GHC.Generics as G
import qualified Data.DList as DL
import qualified Glazier.React as R
import qualified Glazier.React.Event.Internal as RI
import qualified Glazier.React.Framework as F
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P

newtype SetRef = SetRef RI.EventTarget
    deriving (G.Generic, NFData)

-- | Use with 'divideContent' for the builder of @@DL.DList R.Listener@
--
withRef
    :: ( R.MonadReactor x m
       , HasItem' (DL.DList R.Listener) s
       , HasItem' R.EventTarget s
       )
    => F.Prototype m v p s
            (Many '[])
            (Many '[RI.EventTarget])
            x
            (Which '[SetRef])
            (Which '[SetRef])
            (Which '[])
withRef =
    F.Prototype
        ( mempty
        , mempty
        , F.hardcodeItem @(R.EventTarget) (RI.EventTarget $ JE.JSVar J.nullRef)
        , F.triggerExecutor [F.Trigger ("ref", pure . DL.singleton . pick . SetRef . RI.EventTarget . JE.JSVar)]
          `P.pmappend` (F.handlerExecutor' (F.contramapHandlerInput obvious rh))
        )
  where
    rh = F.viaModel (alongside id item') (F.refHandler whenSetRef)

    whenSetRef :: (R.MonadReactor x m)
      => IORef v
      -> Lens' v (F.ComponentPlan x m, R.EventTarget)
      -> SetRef
      -> m (DL.DList (Which '[]))
    whenSetRef ref this (SetRef j) = do
           R.doModifyIORef' ref (set' (this._2) j)
           pure mempty
