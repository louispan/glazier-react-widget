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
import qualified Glazier.React.Framework as F
import qualified GHCJS.Types as J
import qualified Parameterized.Data.Monoid as P

newtype SetRef = SetRef J.JSVal
    deriving (G.Generic, NFData)

-- | Use with 'divideContent' for the builder of @@DL.DList R.Listener@
--
withRef
    :: ( R.MonadReactor x m
       , HasItem' (DL.DList R.Listener) s
       , HasItem' J.JSVal s
       )
    => F.Prototype m v p s
            (Many '[])
            (Many '[J.JSVal])
            x
            (Which '[SetRef])
            (Which '[SetRef])
            (Which '[])
withRef =
    F.Prototype
        ( mempty
        , mempty
        , F.hardcodeItem @(J.JSVal) J.nullRef
        , F.triggerExecutor [F.Trigger ("ref", pure . DL.singleton . pick . SetRef)]
          `P.pmappend` (F.handlerExecutor' (F.contramapHandlerInput obvious rh))
        )
  where
    rh = F.viaModel (alongside id item') (F.refHandler whenSetRef)

    whenSetRef :: (R.MonadReactor x m)
      => IORef v
      -> Lens' v (F.ComponentPlan x m, J.JSVal)
      -> SetRef
      -> m (DL.DList (Which '[]))
    whenSetRef ref this (SetRef j) = do
           R.doModifyIORef' ref (set' (this._2) j)
           pure mempty
