{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Gadgets.Render
    ( Command(..)
    , Action(..)
    , Plan(..)
    , HasPlan(..)
    , mkPlan
    , windowProps
    , gadget
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

data Command gz = RenderCommand gz [JE.Property] J.JSVal

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

windowProps :: HasPlan s => s -> ([JE.Property], [R.Handle])
windowProps s = (mempty, [("ref", s ^. onComponentRef)])

instance CD.Disposing Plan

gadget :: HasPlan giz => G.Gadget Action giz (D.DList (Command giz))
gadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        RenderAction -> D.singleton <$> basicRenderCmd frameNum componentRef RenderCommand

-- | Just change the state to something different so the React pureComponent will call render()
-- renderCmd :: Monad m => (sm -> [JE.Property] -> J.JSVal -> cmd) -> G.GadgetT act sm m cmd
-- The resulting command should be interpreted using 'componentSetState'
basicRenderCmd :: MonadState giz m =>
           Lens' giz Int
           -> Getter giz J.JSVal
           -> (giz -> [JE.Property] -> J.JSVal -> cmd)
           -> m cmd
basicRenderCmd frameNum' componentRef' fcmd = do
    frameNum' %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
    i <- JE.toJS <$> use frameNum'
    r <- use componentRef'
    sm <- get
    pure $ fcmd sm [("frameNum", JE.JSVar i)] r
