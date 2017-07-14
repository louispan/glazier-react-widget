{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Devices.Render
    ( Command(..)
    , Action(..)
    , Plan(..)
    , HasPlan(..)
    , gizmo
    ) where

import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified GHC.Generics as G
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

data Command = forall mdl. RenderCommand (R.Shared mdl) [JE.Property] J.JSVal

data Action
    = ComponentRefAction J.JSVal
    | RenderAction

data Plan = Plan
    { _frameNum :: Int
    , _componentRef :: J.JSVal
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan :: F (R.Maker Action) Plan
mkPlan = Plan
    <$> pure 0
    <*> pure J.nullRef
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)

instance R.Dispose Plan

componentListener :: UniqueMember Plan plns => R.MkComponentListener plns
componentListener plns = D.singleton $ R.ComponentListener ("ref", plns ^. item @Plan . onComponentRef)

gadget :: UniqueMember Plan plns => G.Gadget Action (R.BaseEntity dtls plns) (D.DList Command)
gadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            (pln . componentRef) .= node
            pure mempty

        RenderAction -> do
            -- Just change the state to a different number so the React PureComponent will call render()
            (pln . frameNum) %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
            i <- JE.toJS <$> use (pln . frameNum)
            r <- use (pln . componentRef)
            s <- get
            pure . D.singleton $ RenderCommand s [("frameNum", JE.JSVar i)] r
  where
    pln :: UniqueMember Plan plns => Lens' (R.BaseEntity dtls plns) Plan
    pln = R.plans . item @Plan

gizmo
    :: ( UniqueMember Action acts
       , UniqueMember Plan plns
       , UniqueMember Command cmds
       )
    => R.Gizmo '[Action] '[Plan] '[Command] acts dtls plns cmds
gizmo =
       (single <$> R.hoistWithAction pick mkPlan)
    ./ componentListener
    ./ pure mempty
    ./ (fmap pick <$> magnify (facet @Action) gadget)
    ./ nil
