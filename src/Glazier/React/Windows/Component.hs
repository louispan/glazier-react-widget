{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Windows.Component
    ( Plan(..)
    , HasPlan(..)
    , mkPlan
    , window
    ) where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

data Plan = Plan
    { _component :: R.ReactComponent
    , _key :: J.JSString
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan :: R.HasScene scn mdl pln => G.WindowT scn R.ReactMl ()
    -> MVar scn
    -> F (R.Maker a) Plan
mkPlan render frm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> (R.mkRenderer frm $ const render)

instance CD.Disposing Plan

instance HasPlan pln => HasPlan (R.Scene mdl pln) where
    plan = R.plan . plan

instance HasPlan pln => HasPlan (R.Gizmo mdl pln) where
    plan = R.plan . plan

-- | Exposed to parent components to render this component
window :: (R.HasScene scn mdl pln, HasPlan pln) => (scn -> R.WindowProps) -> G.WindowT scn R.ReactMl ()
window fprops = do
    s <- ask
    let R.WindowProps (props, hdls) = fprops s
    lift $
        R.lf
            (s ^. R.scene . component . to JE.toJS')
            ([ ("key", s ^. R.scene . key . to JE.toJS')
             -- NB. render is not a 'Handle' as it returns an 'IO JSVal'
             , ("render", s ^. R.scene . onRender . to JE.toJS')
             ] ++
             props)
            hdls
