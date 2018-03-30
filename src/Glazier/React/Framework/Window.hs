{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Window where

import Control.Lens
import Control.Monad.Trans.RWSs.Strict
import qualified Data.DList as DL
import Glazier.React
import Glazier.React.Framework.MkId
import Glazier.React.Framework.Scene
import qualified JavaScript.Extras as JE

type WindowT s m = RWSsT (Scene s) () (DL.DList ReactMarkup) m
type Window s = WindowT s Identity


-- type SceneDisplay x s r = Display (Scene x s) r
----------------------------------------------------------------------------------

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for a 'GizmoId'.
lf' :: GizmoId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> DL.DList JE.Property
    -> Window s ()
lf' gid n props = do
    ls <- view (_plan._gizmos.ix gid._listeners)
    leaf ls n props

-- | Interactive version of 'bh'
bh' :: GizmoId
    -> JE.JSRep
    -> DL.DList JE.Property
    -> Window s r
    -> Window s r
bh' gid n props childs = do
    ls <- view (_plan._gizmos.ix gid._listeners)
    branch ls n props childs

-- | Use this to create a display for a top level 'Gadget'
-- Eg. the result of a 'Widget' that has the Window rendering function
-- inserted into 'Glazier.React.Framework.Core.Widget.MkShimListeners'.
shimWindow :: Window s ()
shimWindow = do
    ls <- view (_plan._shimListeners)
    case ls of
        Nothing -> pure ()
        Just (ShimListeners renderCb updatedCb refCb) ->
            -- These are the callbacks on the 'ShimComponent'
            -- See jsbits/react.js
            leaf [] shimComponent
                [ ("render", JE.toJSR renderCb)
                , ("updated", JE.toJSR updatedCb)
                , ("ref", JE.toJSR refCb)
                ]
