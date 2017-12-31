{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Commands.Focus where

import Control.Lens
import Control.DeepSeq
import Data.Diverse.Lens
import Data.IORef
import qualified GHC.Generics as G
import qualified Data.DList as DL
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands.Rerender as C

newtype Focus = Focus R.EventTarget
    deriving (G.Generic, NFData)

-- | Need to delay focusing until after the next render
focus :: (R.MonadReactor x m)
  => (DL.DList (Which '[Focus]) -> m (DL.DList x))
  -> IORef v
  -> Lens' v (F.ComponentPlan x m, R.EventTarget)
  -> m C.Rerender
focus k ref this = C.mkRerender ref (this._1) (do
    obj <- R.doReadIORef ref
    k $ DL.singleton $ pick (Focus (obj ^. this._2)))
