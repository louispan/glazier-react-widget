{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Devices.Render
    ( Command(..)
    , Action(..)
    , Plan(..)
    , HasPlan(..)
    , Device
    , device
    ) where

import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Control.Disposable as CD
import qualified Data.DList as D
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified GHC.Generics as G
import qualified Glazier as G
import qualified Glazier.React as R
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

instance CD.Disposing Plan

windowAttributes :: Lens' mdl Plan -> mdl -> R.WindowAttributes
windowAttributes pln mdl = R.WindowAttributes (mempty, [("ref", mdl ^. pln . onComponentRef)])

gadget :: Lens' mdl Plan -> G.Gadget Action (R.Shared mdl) (D.DList Command)
gadget pln = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            (R.ival . pln . componentRef) .= node
            pure mempty

        RenderAction -> do
            -- Just change the state to a different number so the React pureComponent will call render()
            (R.ival . pln . frameNum) %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
            i <- JE.toJS <$> use (R.ival . pln . frameNum)
            r <- use (R.ival . pln . componentRef)
            s <- get
            pure . D.singleton $ RenderCommand s [("frameNum", JE.JSVar i)] r

type Device mdl = R.Device Action Plan Command mdl

device :: Lens' mdl Plan -> Device mdl
device pln = R.Device pln mkPlan (gadget pln) (windowAttributes pln)
