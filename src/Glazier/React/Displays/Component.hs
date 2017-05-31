{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Displays.Component
    ( Plan(..)
    , HasPlan(..)
    , Display
    , display
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
    , _onRender ::  J.Callback (IO J.JSVal)
    } deriving (G.Generic)

makeClassy ''Plan

mkComponentPlan
    :: G.WindowT mdl R.ReactMl () -> MVar mdl -> F (R.Maker a) Plan
mkComponentPlan render frm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> (R.mkRenderer render frm)

instance CD.Disposing Plan

-- | Exposed to parent components to render this component
window :: Lens' mdl Plan -> (mdl -> R.ComponentAttributes) -> G.WindowT mdl R.ReactMl ()
window pln wa = do
    s <- ask
    let R.ComponentAttributes (props, hdls) = wa s
    lift $
        R.lf
            (s ^. pln . component . to JE.toJS')
            ([ ("key", s ^. pln . key . to JE.toJS')
             -- NB. render is not a 'R.Handle' as it returns an 'IO JSVal'
             , ("render", s ^. pln . onRender . to JE.toJS')
             ] ++
             props)
            hdls

type Display a mdl = R.Display a Plan mdl

display
    :: Lens' mdl Plan
    -> G.WindowT mdl R.ReactMl ()
    -> (mdl -> R.ComponentAttributes)
    -> Display a mdl
display pln render ca = R.Display pln (mkComponentPlan render) (window pln ca)
