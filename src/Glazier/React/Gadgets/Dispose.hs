{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Gadgets.Dispose
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
import qualified Control.Disposable as CD
import qualified Data.DList as D
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified GHC.Generics as G
import qualified Glazier as G
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

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

windowProps :: HasPlan s => s -> [JE.Property]
windowProps s = [("componentDidUpdate", s ^. onComponentDidUpdate . to JE.toJS')]

instance CD.Disposing Plan

gadget :: HasPlan giz => G.Gadget Action giz (D.DList Command)
gadget = do
    a <- ask
    case a of
        DisposeAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            ds <- use deferredDisposables
            deferredDisposables .= mempty
            pure . D.singleton . DisposeCommand . CD.DisposeList $ D.toList ds
