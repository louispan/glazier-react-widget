module Glazier.React.Framework.Effect.DoEffect where

import Control.Lens
import Data.Diverse.Lens
import qualified Glazier.React as R

doEffect' ::
    ( R.MonadReactor x m
    , AsFacet e x
    )
    => e -> m ()
doEffect' e = R.doEffect (review facet e)
