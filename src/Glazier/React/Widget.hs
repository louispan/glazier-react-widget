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
import Data.Coerce

----------------------------------------------------------

-- | Something that has an immutable component, as well as a TMVar that
-- can be used to share a value with other threads.
-- This is used by the gadget to be able to purely manipulate a value
-- as well as put it into an TMVar for other threads to access the value.
newtype Shared a = Shared { runShared :: (TMVar a,  a) }

instance R.Dispose a => R.Dispose (Shared a) where
    dispose (Shared (_, a)) = R.dispose a

_Shared :: Iso' (Shared a) (TMVar a,  a)
_Shared = iso runShared Shared

tmvar :: Lens' (Shared a) (TMVar a)
tmvar = _Shared . _1

ival :: Lens' (Shared a) a
ival = _Shared . _2

-- class Shared c a | c -> a where
--     tmvar :: Lens' c (TMVar a)
--     ival :: Lens' c a

-- instance Shared (TMVar a, a) a where
--     tmvar = _1
--     ival = _2

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

class HasComponentListeners c plns | c -> plns where
    componentListeners :: Lens' c (Many plns -> D.DList R.Listener)

class HasComponentProperties c dtls plns | c -> plns, c -> dtls where
    componentProperties :: Lens' c (BaseModel dtls plns -> D.DList JE.Property)

----------------------------------------------------------

-- | A record of Detail and Plan and ComponentPlan
newtype BaseModel (dtls :: [Type]) (plns :: [Type]) = BaseModel
    { runBaseModel ::
        ( Many plns -> D.DList R.Listener
        , BaseModel dtls plns -> D.DList JE.Property
        , Many dtls
        , Many plns
        , ComponentPlan
        )
    }
    deriving (G.Generic)

instance (R.Dispose (Many dtls), R.Dispose (Many plns)) => R.Dispose (BaseModel dtls plns) where
    dispose (BaseModel (ls, props, dtls, plns, compPln)) = R.dispose (dtls, plns, compPln)

_BaseModel :: Iso'
    (BaseModel dtls plns)
    ( Many plns -> D.DList R.Listener
    , BaseModel dtls plns -> D.DList JE.Property
    , Many dtls
    , Many plns
    , ComponentPlan
    )
_BaseModel = iso runBaseModel BaseModel

instance HasComponentListeners (BaseModel dtls plns) plns where
    componentListeners = _BaseModel . _1

instance HasComponentProperties (BaseModel dtls plns) dtls plns where
    componentProperties = _BaseModel . _2

instance HasDetails (BaseModel dtls plns) dtls where
    details = _BaseModel . _3

instance HasPlans (BaseModel dtls plns) plns where
    plans = _BaseModel . _4

instance HasComponentPlan (BaseModel dtls plns) where
    componentPlan = _BaseModel . _5

----------------------------------------------------------

instance HasComponentListeners (Shared (BaseModel dtls plns)) plns where
    componentListeners = ival . componentListeners

instance HasComponentProperties (Shared (BaseModel dtls plns)) dtls plns where
    componentProperties = ival . componentProperties

instance HasDetails (Shared (BaseModel dtls plns)) dtls where
    details = ival . details

instance HasPlans (Shared (BaseModel dtls plns)) plns where
    plans = ival . plans

instance HasComponentPlan (Shared (BaseModel dtls plns)) where
    componentPlan = ival . componentPlan

-- ----------------------------------------------------------

-- -- newtype Plans plns = Plans { runPlans :: Many plns }
-- newtype ComponentProperties plns = ComponentProperties
--     { runComponentProperties :: BaseModel dtl plns -> D.DList JE.Property
--     } deriving (Semigroup, Monoid)

-- newtype ComponentListeners plns = ComponentListeners
--     { runComponentListeners :: Many plns -> D.DList R.Listener
--     } deriving (Semigroup, Monoid)

----------------------------------------------------------

componentWindow
    :: G.WindowT (BaseModel dtls plns) R.ReactMl ()
componentWindow = do
    s <- ask
    lift $
        R.lf
            (s ^. componentPlan  . component . to JE.toJS')
            ( [ ("key", s ^. componentPlan . key . to JE.toJS')
               -- NB. render is not a 'R.Handle' as it returns an 'IO JSVal'
              , ("render", s ^. componentPlan . onRender . to JE.toJS')
              ] ++ (D.toList $ (s ^. componentProperties) s))
            (D.toList $ (s ^. componentListeners) (s ^. plans))

-- | Make a BaseEntity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkBaseEntity
    :: (mdl ~ BaseModel dtls plns)
    => (Many plns -> D.DList R.Listener)
    -> (BaseModel dtls plns -> D.DList JE.Property)
    -> Many dtls
    -> F (R.Maker act) (Many plns)
    -> G.WindowT mdl R.ReactMl ()
    -> F (R.Maker act) (Shared mdl)
mkBaseEntity ls props dtls mkPlns render = do
    frm <- R.mkEmptyFrame
    mdl <- (\plns compPln -> BaseModel (ls, props, dtls, plns, compPln)) <$> mkPlns <*> mkComponentPlan render frm
    R.putFrame frm mdl
    pure (Shared (frm, mdl))

-- data Widget act dtls plns cmd mdl where
--     Widget ::
--         -- (R.Dispose (Many plns), R.Dispose dtl) =>
--         (F (R.Maker act) (Many plns))
--         -> G.WindowT mdl R.ReactMl ()
--         -> G.Gadget act (Shared mdl) (D.DList cmd)
--         -> (mdl -> D.DList JE.Property)
--         -> (mdl -> D.DList R.Listener)
--         -> Widget act dtls plns cmd mdl
