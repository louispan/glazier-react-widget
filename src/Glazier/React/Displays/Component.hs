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
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan
    :: (J.JSVal -> G.WindowT mdl R.ReactMl ()) -> MVar mdl -> F (R.Maker a) Plan
mkPlan render frm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> (R.mkRenderer frm render)

instance CD.Disposing Plan

-- | Exposed to parent components to render this component
window :: (R.HasPlan mdl pln, HasPlan pln) => (mdl -> R.WindowAttrs) -> G.WindowT mdl R.ReactMl ()
window wa = do
    s <- ask
    let R.WindowAttrs (props, hdls) = wa s
    lift $
        R.lf
            (s ^. R.plan . component . to JE.toJS')
            ([ ("key", s ^. R.plan . key . to JE.toJS')
             -- NB. render is not a 'Handle' as it returns an 'IO JSVal'
             , ("render", s ^. R.plan . onRender . to JE.toJS')
             ] ++
             props)
            hdls

type Display a mdl = R.Display a Plan mdl

display :: (HasPlan pln, R.HasPlan mdl pln) => (J.JSVal -> G.WindowT mdl R.ReactMl ())
     -> (mdl -> R.WindowAttrs) -> R.Display a Plan mdl
display render wa = R.Display (mkPlan render) (const $ window wa)
