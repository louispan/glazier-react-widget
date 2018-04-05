{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Window where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Trans.RWSs.Strict
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Framework.MkId
import Glazier.React.Framework.Scene
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE

type WindowT s m = RWSsT (Scene s) () (DL.DList ReactMarkup) m
type Window s = WindowT s Identity
type WindowSTM s = WindowT s STM

-- type SceneDisplay x s r = Display (Scene x s) r
----------------------------------------------------------------------------------

getListeners :: GizmoId -> Window s [JE.Property]
getListeners gid = do
    cb <- preview (_plan._shimCallbacks._Just._onListen)
    case cb of
        Nothing -> pure mempty
        Just cb' -> do
            ks <- view (_plan._gizmos.ix gid._listeners2.to M.keys)
            let go k = (k, bindListenerContext (context k) cb')
            pure (go <$> ks)
  where
    context k = JE.toJSR $ JA.fromList [JE.toJS gid, JE.toJS k]

-- | Interactive version of 'lf' using listeners obtained from the 'Plan' for a 'GizmoId'.
lf' :: GizmoId
    -> JE.JSRep -- ^ eg "div" or "input"
    -> DL.DList JE.Property
    -> Window s ()
lf' gid n props = do
    ls <- getListeners gid
    leaf n (props <> DL.fromList ls)

-- | Interactive version of 'bh'
bh' :: GizmoId
    -> JE.JSRep
    -> DL.DList JE.Property
    -> Window s r
    -> Window s r
bh' gid n props childs = do
    ls <- getListeners gid
    branch n (props <> DL.fromList ls) childs

-- | Use this to create a display for a top level 'Gadget'
-- Eg. the result of a 'Widget' that has the Window rendering function
-- inserted into 'Glazier.React.Framework.Core.Widget.MkShimListeners'.
shimWindow :: Window s ()
shimWindow = do
    ls <- view (_plan._shimCallbacks)
    case ls of
        Nothing -> pure ()
        Just (ShimCallbacks renderCb updatedCb refCb _) ->
            -- These are the callbacks on the 'ShimComponent'
            -- See jsbits/react.js
            leaf shimComponent
                [ ("render", JE.toJSR renderCb)
                , ("updated", JE.toJSR updatedCb)
                , ("ref", JE.toJSR refCb)
                ]


bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
bindListenerContext = js_bindListenerContext

#ifdef __GHCJS__

-- -- | Combine functions into a single function
-- -- Given two 'Callback (JSVal -> IO ())'
-- -- return a function that calls both callbacks
-- foreign import javascript unsafe
--     "$r = function(j) { $1(j); $2(j); };"
--     js_combineFunction1 ::
--         JE.JSRep
--         -> JE.JSRep
--         -> JE.JSRep

foreign import javascript unsafe
    "$r = function(j) { $2($1, j) };"
    js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep

#else

-- js_combineFunction1 ::
--     JE.JSRep
--     -> JE.JSRep
--     -> JE.JSRep
-- js_combineFunction1 = const

js_bindListenerContext :: JE.JSRep -> J.Callback (J.JSVal -> J.JSVal -> IO ()) -> JE.JSRep
js_bindListenerContext _ _ = JE.JSRep J.nullRef


#endif
