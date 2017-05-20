{-# LANGUAGE DeriveGeneric #-}
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

instance HasPlan (R.Scene s Plan) where
    plan = R.plan

-- | Exposed to parent components to render this component
window :: HasPlan s => [JE.Property] -> G.WindowT s R.ReactMl ()
window props = do
    s <- ask
    lift . R.lf (s ^. component . to JE.toJS') $
        [ ("key", s ^. key . to JE.toJS')
        , ("render", s ^. onRender . to JE.toJS')
        ] ++
        props
