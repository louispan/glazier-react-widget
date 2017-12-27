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
    , HasItem' (DL.DList JE.Property) is
    , HasItem' (DL.DList JE.Property) ss
    , HasItem' (DL.DList R.Listener) ss
    ) => F.Prototype m v is ss (Many is') (Many ss') x c a b
    -> F.Prototype m v is ss
        (Many ((DL.DList JE.Property) ': is'))
        (Many ((DL.DList R.Listener) ': (DL.DList JE.Property) ': ss'))
        x c a b
divideContent (F.Prototype (F.Display disp, F.Builder (F.MkInfo mkInf, F.MkModel mkMdl), exec)) =
    F.Prototype
        ( F.Display $ \(cp, ss) -> do
                R.bh "div"
                    (DL.toList $ view (item' @(DL.DList R.Listener)) ss)
                    (DL.toList $ view (item' @(DL.DList JE.Property)) ss)
                    (disp (cp, ss))
        , F.Builder ( F.MkInfo $ \ss -> (\i -> (ss ^. item' @(DL.DList JE.Property)) ./ i)
                        <$> mkInf ss
                    , F.MkModel $ \is -> (\s -> mempty ./ (is ^. item' @(DL.DList JE.Property)) ./ s)
                        <$> mkMdl is
                    )
        , exec)
