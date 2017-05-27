{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Devices.Dispose
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
import qualified Control.Disposable as CD
import qualified Data.DList as D
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified GHC.Generics as G
import qualified Glazier as G
import qualified Glazier.React as R

newtype Command = DisposeCommand CD.SomeDisposable

data Action = DisposeAction

data Plan = Plan
    { _deferredDisposables :: D.DList CD.SomeDisposable
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan :: F (R.Maker Action) Plan
mkPlan = Plan
    <$> pure mempty
    <*> (R.mkHandler $ pure . pure . const DisposeAction)

instance CD.Disposing Plan

windowAttrs :: (R.HasPlan mdl pln, HasPlan pln) => mdl -> R.WindowAttrs
windowAttrs mdl = R.WindowAttrs (mempty, [("componentDidUpdate", mdl ^. R.plan . onComponentDidUpdate)])

gadget :: (R.HasPlan mdl pln, HasPlan pln) => G.Gadget Action (R.Shared mdl) (D.DList Command)
gadget = do
    a <- ask
    case a of
        DisposeAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            ds <- use (R.plan . deferredDisposables)
            (R.plan . deferredDisposables) .= mempty
            pure . D.singleton . DisposeCommand . CD.DisposeList $ D.toList ds

type Device mdl = R.Device Action Plan Command mdl

device :: (R.HasPlan mdl pln, HasPlan pln) => R.Device Action Plan Command mdl
device = R.Device (const mkPlan) gadget windowAttrs (const mempty)
