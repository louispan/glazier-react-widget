{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widgets.Input where

import Control.Lens
import Control.Monad.Reader
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands.Property as C
import qualified JavaScript.Extras as JE

data InputAction
    = SubmitAction R.EventTarget J.JSString
    | CancelAction R.EventTarget

wack
    :: (UniqueMember InputAction acts, UniqueMember C.PropertyCommand cmds)
    => F.Prototype '[] ols '[] dtls '[] plns '[InputAction] acts '[C.PropertyCommand] cmds
wack = inputPrototype1 F.+<>+ inputPrototype

inputPrototype1 :: F.Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds
inputPrototype1 = (F.statically $ F.display d)
  where
    d ls ps = lift $ R.lf "input" ls ps

inputPrototype
    :: (UniqueMember InputAction acts, UniqueMember C.PropertyCommand cmds)
    => F.Prototype '[] ols '[] dtls '[] plns '[InputAction] acts '[C.PropertyCommand] cmds
-- inputPrototype = (F.statically $ F.display d) F.+<>+ g'
inputPrototype = g''
  where
    -- d ls ps = lift $ R.lf "input" ls ps
    g' :: (UniqueMember InputAction acts, UniqueMember C.PropertyCommand cmds) => G.Gadget (Which acts) (F.Entity dtls plns) (D.DList (Which cmds))
    g' = F.gadgetry g
    g'' = F.dynamically (F.gizmo g')
    g :: G.Gadget InputAction (F.Entity dtls plns) (D.DList C.PropertyCommand)
    g = do
        a <- ask
        case a of
            CancelAction j -> pure $ D.singleton $ C.SetPropertyCommand (JE.toJS j) ("value", JE.toJS' J.empty)
            _ -> pure mempty

-- render :: Lens' mdl Detail -> Lens' mdl Plan -> (mdl -> R.RenderAttributes) -> G.WindowT mdl R.ReactMl ()
-- render dtl pln ra = do
--     s <- ask
--     let R.RenderAttributes (props, hdls) = ra s
--     lift $
--         R.lf
--             "input"
--             ([ ("key", s ^. pln . D.Component.key . to JE.toJS')
--              , ("className", s ^. dtl . className . to JE.toJS')
--              , ("placeholder", s ^. dtl . placeholder . to JE.toJS')
--              , ("autoFocus", s ^. dtl . autoFocus . to JE.toJS')
--              ] ++
--              props)
--             ([ ("onKeyDown", s ^. pln . onKeyDown)
--              -- , ("onChanged", s ^. pln . onChanged)
--              ] ++
--              hdls)

-- whenKeyDown :: J.JSVal -> MaybeT IO (R.EventTarget, Maybe J.JSString)
-- whenKeyDown evt = do
--         syn <- MaybeT $ pure $ JE.fromJS evt
--         kevt <- MaybeT $ pure $ R.parseKeyboardEvent syn
--         let evt' = R.parseEvent syn
--             k = R.key kevt
--         target <- lift $ pure . R.target $ evt'
--         case k of
--             "Escape" -> pure (target, Nothing)
--             "Enter" -> do
--                 v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
--                 pure (target, Just v)
--             _ -> empty

-- onKeyDown' :: J.JSVal -> MaybeT IO [Action]
-- onKeyDown' = R.eventHandlerM whenKeyDown lazily
--   where
--     lazily :: (R.EventTarget, Maybe J.JSString) -> MaybeT IO [Action]
--     lazily (j, ms) = pure $
--         maybe [CancelAction j] (pure . SubmitAction j) ms

-- -- onChanged' :: J.JSVal -> MaybeT IO [Action]
-- -- onChanged' = R.eventHandlerM strictly lazily
-- --   where
-- --     strictly evt = do
-- --         target <- MaybeT . pure $ (JE.fromJS evt) <&> (R.target . R.parseEvent)
-- --         v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
-- --         pure (target, v)
-- --     lazily :: (R.EventTarget, J.JSString) -> MaybeT IO [Action]
-- --     lazily (j, s) = pure [ChangedAction j s]

-- type Widget mdl = R.Widget Action Outline Detail Plan Command mdl

-- widget
--     :: Lens' mdl Detail
--     -> Lens' mdl Plan
--     -> (mdl -> R.RenderAttributes)
--     -> Widget mdl
-- widget dtl pln ra = R.Widget
--     dtl
--     pln
--     outline
--     mkDetail
--     (mkComponentPlan component')
--     (R.window component')
--     gadget
--   where
--     component' = D.Component.display (pln . componentPlan) (render dtl pln ra) mempty
