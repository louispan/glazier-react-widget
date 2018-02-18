{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Framework.Effect.SetProperty where

import Data.Diverse.Lens
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Effect.DoEffect as F
import qualified JavaScript.Extras as JE

data SetProperty = SetProperty JE.Property J.JSVal

setProperty ::
    ( R.MonadReactor x m
    , AsFacet SetProperty x
    )
    => JE.Property -> J.JSVal -> m ()
setProperty p j = F.doEffect' $ SetProperty p j
