{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Framework.Effect.GetProperty where

import Data.Diverse.Lens
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Effect.DoEffect as F
import qualified JavaScript.Extras as JE

data GetProperty m = GetProperty J.JSString J.JSVal (JE.JSVar -> m ())

getProperty ::
    ( R.MonadReactor x m
    , AsFacet (GetProperty m) x
    )
    => J.JSString -> J.JSVal -> (JE.JSVar -> m ()) -> m ()
getProperty p j k = F.doEffect' $ GetProperty p j k
