-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Display where

-- import Control.Lens
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Model as F

type Display m s r = (s -> R.ReactMlT m r)

type FrameDisplay x m s r = Display m (F.Frame x m s) r

-- newtype DisplayOnSpec m r s = DisplayOnSpec { runDisplayOnSpec :: Display m s r }

-- type instance F.OnSpec (DisplayOnSpec m r) s = Display m s r

-- instance F.ViaSpec (DisplayOnSpec m r) where
--     viaSpec l f = f . view l
