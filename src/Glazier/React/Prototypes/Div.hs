{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Prototypes.Div where

import Control.Lens
import qualified Data.DList as DL
import Data.Diverse.Profunctor
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified JavaScript.Extras as JE

-- | Wrap a prototype inside a "div", and adds @DL.DList JE.Property@ and @DL.DList R.Listener@
-- to the model.
divideContent ::
    ( Monad m
    , HasItem' (DL.DList JE.Property) ps
    , HasItem' (DL.DList JE.Property) ss
    , HasItem' (DL.DList R.Listener) ss
    ) => F.Prototype m v ps ss (Many ps') (Many ss') x c a b
    -> F.Prototype m v ps ss
        (Many ((DL.DList JE.Property) ': ps'))
        (Many ((DL.DList R.Listener) ': (DL.DList JE.Property) ': ss'))
        x c a b
divideContent (F.Prototype (F.Display disp, F.Builder (F.MkPlan mkPln, F.MkModel mkMdl), exec)) =
    F.Prototype
        ( F.Display $ \(cm, ss) -> do
                R.bh "div"
                    (DL.toList $ view (item' @(DL.DList R.Listener)) ss)
                    (DL.toList $ view (item' @(DL.DList JE.Property)) ss)
                    (disp (cm, ss))
        , F.Builder ( F.MkPlan $ \ps -> (\p -> (ps ^. item' @(DL.DList JE.Property)) ./ p)
                        <$> mkPln ps
                    , F.MkModel $ \ss -> (\p -> mempty ./ (ss ^. item' @(DL.DList JE.Property)) ./ p)
                        <$> mkMdl ss
                    )
        , exec)
