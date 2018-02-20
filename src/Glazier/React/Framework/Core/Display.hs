-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Display where

import Control.Lens
import Data.Generics.Product
import Data.Maybe
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Model as F
import qualified JavaScript.Extras as JE

type Display m s r = (s -> R.ReactMlT m r)

type FrameDisplay m s r = Display m (F.Frame m s) r

-- | Create an intteractive DOM element
-- by wrapping the provided display of inside a provided 'name',
-- and attaching listeners from the plan.
gadget :: (Monad m)
    => F.GadgetId
    -> J.JSString
    -> (F.Frame m s -> [JE.Property])
    -> FrameDisplay m s r
    -> FrameDisplay m s r
gadget i n f disp s =
    let props = f s
        ls = fromMaybe [] (s ^. F.plan.field @"listeners".at i)
    in R.branch (JE.toJS' n)
            ls
            props
            (disp s)
