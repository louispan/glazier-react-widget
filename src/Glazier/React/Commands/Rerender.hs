-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Glazier.React.Commands.Rerender where

-- import qualified GHCJS.Types as J
-- import qualified GHCJS.Marshal.Pure as J
import Control.Lens
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
import qualified JavaScript.Extras as JE

data Rerender = Rerender R.ReactComponent [JE.Property]

mkRerender :: R.MonadReactor m => IORef v -> Lens' v F.ComponentModel -> m Rerender
mkRerender ref this = do
    obj <- R.doReadIORef ref
    let (i, obj') = obj & this.F.componentFrameNum <%~ ((+ 1) . (`mod` JE.maxSafeInteger))
    R.doWriteIORef ref obj'
    pure $ Rerender (obj ^. this.F.component) [("frameNum", JE.toJS' i)]
