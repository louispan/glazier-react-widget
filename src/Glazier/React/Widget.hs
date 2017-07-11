{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Widget where

import Control.Applicative
import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Kind
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

----------------------------------------------------------

-- | ComponentPlan has to be stored differently to other plans because mkComponentPlan needs
-- additional parameters
data ComponentPlan = ComponentPlan
    { _component :: R.ReactComponent
    , _key :: J.JSString
    , _onRender ::  J.Callback (IO J.JSVal)
    } deriving (G.Generic)

makeClassy ''ComponentPlan

mkComponentPlan
    :: G.WindowT mdl R.ReactMl () -> TMVar mdl -> F (R.Maker act) ComponentPlan
mkComponentPlan render frm = ComponentPlan
    <$> R.getComponent
    <*> R.mkKey
    <*> (R.mkRenderer render frm)

instance R.Dispose ComponentPlan

----------------------------------------------------------

class HasDetails c dtls | c -> dtls where
    details :: Lens' c (Many dtls)

class HasPlans c plns | c -> plns where
    plans :: Lens' c (Many plns)

----------------------------------------------------------

newtype BaseModel (dtls :: [Type]) (plns :: [Type]) = BaseModel
    { runBaseModel ::
        ( Many dtls
        , Many plns
        , ComponentPlan
        )
    }
    deriving G.Generic

instance (R.Dispose (Many dtls), R.Dispose (Many plns)) => R.Dispose (BaseModel dtls plns)

instance HasDetails (BaseModel dtls plns) dtls where
    details = _BaseModel . _1

instance HasPlans (BaseModel dtls plns) plns where
    plans = _BaseModel . _2

instance HasComponentPlan (BaseModel dtls plns) where
    componentPlan = _BaseModel . _3

_BaseModel :: Iso'
    (BaseModel dtls plns)
    ( Many dtls
    , Many plns
    , ComponentPlan
    )
_BaseModel = iso runBaseModel BaseModel

----------------------------------------------------------

-- | Something that has an immutable component, as well as a TMVar that
-- can be used to share a value with other threads.
-- This is used by the gadget to be able to purely manipulate a value
-- as well as put it into an TMVar for other threads to access the value.
newtype Shared a = Shared
    { runShared :: (TMVar a, a)
    } deriving G.Generic

instance R.Dispose a => R.Dispose (Shared a) where
    dispose (Shared (_, a)) = R.dispose a

_Shared :: Iso' (Shared a) (TMVar a,  a)
_Shared = iso runShared Shared

tmvar :: Lens' (Shared a) (TMVar a)
tmvar = _Shared . _1

ival :: Lens' (Shared a) a
ival = _Shared . _2

type BaseEntity dtls plns = Shared (BaseModel dtls plns)

instance HasDetails (BaseEntity dtls plns) dtls where
    details = ival . details

instance HasPlans (BaseEntity dtls plns) plns where
    plans = ival . plans

instance HasComponentPlan (BaseEntity dtls plns) where
    componentPlan = ival . componentPlan

----------------------------------------------------------

type MkPlan acts pln = F (R.Maker (Which acts)) pln
type MkListener plns = Many plns -> D.DList R.Listener
type MkProperty dtls plns = BaseModel dtls plns -> D.DList JE.Property
type Window dtls plns = G.WindowT (BaseModel dtls plns) R.ReactMl ()
type Gadget acts dtls plns cmds = G.Gadget (Which acts) (BaseEntity dtls plns) (D.DList (Which cmds))

type Widget pln acts dtls plns cmds = Many
                    '[ MkPlan acts pln
                     , MkListener plns
                     , MkProperty dtls plns
                     , Window dtls plns
                     , Gadget acts dtls plns cmds
                     ]

combine
    :: forall pln plns' acts dtls plns cmds.
       Widget (Many plns') acts dtls plns cmds
    -> Widget pln acts dtls plns cmds
    -> Widget (Many (pln ': plns')) acts dtls plns cmds
combine x y = x
    & item' @(MkPlan acts (Many plns')) %~ (\mkPlans -> (./) <$> (y ^. (item @(MkPlan acts pln))) <*> mkPlans)
    & item @(MkListener plns) %~ (<> (y ^. (item @(MkListener plns))))
    & item @(MkProperty dtls plns) %~ (<> (y ^. (item @(MkProperty dtls plns))))
    & item @(Window dtls plns) %~ (<> (y ^. (item @(Window dtls plns))))
    & item @(Gadget acts dtls plns cmds) %~ (<> (y ^. (item @(Gadget acts dtls plns cmds))))

-- | Difference is gadget is combined using <|> instead of <>
fallback
    :: forall pln plns' acts dtls plns cmd.
       Widget (Many plns') acts dtls plns cmd
    -> Widget pln acts dtls plns cmd
    -> Widget (Many (pln ': plns')) acts dtls plns cmd
fallback x y = x
    & item' @(MkPlan acts (Many plns')) %~ (\mkPlans -> (./) <$> (y ^. (item @(MkPlan acts pln))) <*> mkPlans)
    & item @(MkListener plns) %~ (<> (y ^. (item @(MkListener plns))))
    & item @(MkProperty dtls plns) %~ (<> (y ^. (item @(MkProperty dtls plns))))
    & item @(Window dtls plns) %~ (<> (y ^. (item @(Window dtls plns))))
    & item @(Gadget acts dtls plns cmd) %~ (<|> (y ^. (item @(Gadget acts dtls plns cmd))))

----------------------------------------------------------

componentWindow
    :: D.DList R.Listener -> D.DList JE.Property -> G.WindowT (BaseModel dtls plns) R.ReactMl ()
componentWindow ls ps = do
    s <- ask
    lift $
        R.lf
            (s ^. componentPlan  . component . to JE.toJS')
            ((D.fromList [ ("key", s ^. componentPlan . key . to JE.toJS')
                                     -- NB. render is not a 'R.Handle' as it returns an 'IO JSVal'
                                     , ("render", s ^. componentPlan . onRender . to JE.toJS')
                                     ]) <> ps)
            ls

-- | Make a BaseEntity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkBaseEntity
    :: Many dtls
    -> F (R.Maker act) (Many plns)
    -> G.WindowT (BaseModel dtls plns) R.ReactMl ()
    -> F (R.Maker act) (BaseEntity dtls plns)
mkBaseEntity dtls mkPlns render = do
    frm <- R.mkEmptyFrame
    mdl <- (\plns compPln -> BaseModel (dtls, plns, compPln)) <$> mkPlns <*> mkComponentPlan render frm
    R.putFrame frm mdl
    pure (Shared (frm, mdl))
