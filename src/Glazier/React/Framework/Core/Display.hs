-- {-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Display where

import Control.Lens
import qualified Data.DList as DL
import Data.Generics.Product
import Data.Maybe
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Model as R
import qualified JavaScript.Extras as JE

type Display m s r = s -> R.ReactMlT m r

type FrameDisplay m s r = Display m (R.Frame m s) r

-- | Gets the listeners for a particular 'GadgetId', usually for a
-- specific DOM element.
getListeners :: R.GadgetId -> R.Frame m s -> (DL.DList R.Listener)
getListeners i s = fromMaybe DL.empty (s ^. R.plan.field @"listeners".at i)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
lf'
    :: Monad m
    => R.GadgetId
    -> R.Frame m s
    -> JE.JSRep
    -> [JE.Property]
    -> R.ReactMlT m ()
lf' i s n ps = R.leaf (getListeners i s) n (DL.fromList ps)

-- | Convenience function to create an internactive dom element
-- using listenres obtained from the 'Frame' for a 'GadgetId'.
-- Memonic: the convenient listener version has a prime'.
bh'
    :: Monad m
    => R.GadgetId
    -> R.Frame m s
    -> JE.JSRep
    -> [JE.Property]
    -> R.ReactMlT m a
    -> R.ReactMlT m a
bh' i s n ps = R.branch (getListeners i s) n (DL.fromList ps)
