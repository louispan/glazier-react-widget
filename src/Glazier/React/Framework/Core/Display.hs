-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Glazier.React.Framework.Core.Display where

import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Model as R

type Display m s r = (s -> R.ReactMlT m r)

type FrameDisplay m s r = Display m (R.Frame m s) r
