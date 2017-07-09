{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Displays.Component
    ( Plan(..)
    -- , HasPlan(..)
    -- , Display
    -- , display
    ) where

import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Data.Diverse
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

-- wack :: G.WindowT (R.BaseModel dtl plns) R.ReactMl () -> TMVar (R.BaseModel dtl plns) -> F (R.Maker a) (Many plns) -> F (R.Maker a) (Many (Plan ': plns))
-- wack render frm plns = (./) <$> mkComponentPlan render frm <*> plns

-- newtype PlanMaker act plns = PlanMaker (forall act mdl. G.WindowT mdl R.ReactMl () -> TMVar mdl -> F (R.Maker act) (Many plns))


-- wock :: 

-- type Display a mdl = R.Display a Plan mdl

-- display
--     :: Lens' mdl Plan
--     -> G.WindowT mdl R.ReactMl ()
--     -> (mdl -> R.ComponentAttributes)
--     -> Display a mdl
-- display pln render ca = R.Display pln (mkComponentPlan render) (window pln ca)
