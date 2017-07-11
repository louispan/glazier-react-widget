{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Devices.Dispose
    ( Command(..)
    , Action(..)
    , Plan(..)
    , HasPlan(..)
    , widget
    ) where

import Control.Lens
import Control.Monad.Free.Church
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified GHC.Generics as G
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Widget as R

newtype Command = DisposeCommand (R.Disposable ())

data Action = DisposeAction

data Plan = Plan
    { _deferredDisposables :: R.Disposable ()
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''Plan

mkPlan :: F (R.Maker Action) Plan
mkPlan = Plan
    <$> pure mempty
    <*> (R.mkHandler $ pure . pure . const DisposeAction)

instance R.Dispose Plan

componentListener :: UniqueMember Plan plns => R.MkListener plns
componentListener plns = D.singleton ("componentDidUpdate", plns ^. item @Plan . onComponentDidUpdate)

gadget :: UniqueMember Plan plns => G.Gadget Action (R.BaseEntity dtls plns) (D.DList Command)
gadget = do
    -- Run delayed commands that need to wait until frame is re-rendered
    -- Eg focusing after other rendering changes
    ds <- use (pln . deferredDisposables)
    (pln . deferredDisposables) .= mempty
    pure . D.singleton . DisposeCommand $ ds
  where
    pln :: UniqueMember Plan plns => Lens' (R.BaseEntity dtls plns) Plan
    pln = R.plans . item @Plan

widget
    :: ( UniqueMember Action acts
       , UniqueMember Plan plns
       , UniqueMember Command cmds
       )
    => R.Widget Plan acts dtls plns cmds
widget =
       R.hoistWithAction pick mkPlan
    ./ componentListener
    ./ pure mempty
    ./ mempty
    ./ (fmap pick <$> magnify (facet @Action) gadget)
    ./ nil
