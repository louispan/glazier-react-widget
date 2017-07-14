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
{-# LANGUAGE PolyKinds #-}
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
import Data.Coerce
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
-- Two widgets can only be composed monoidally by converting to components (componentWindow
-- Gizmos can be added to Widgets or Gizmos

newtype ComponentListener = ComponentListener { runComponentListener :: R.Listener }
newtype ComponentProperty = ComponentProperty { runComponentProperty :: JE.Property }

type MkPlan acts plns' = F (R.Maker (Which acts)) (Many plns')
type MkComponentListener plns = Many plns -> D.DList ComponentListener
type MkComponentProperty dtls plns = BaseModel dtls plns -> D.DList ComponentProperty
type Gadget acts dtls plns cmds = G.Gadget (Which acts) (BaseEntity dtls plns) (D.DList (Which cmds))
type Window dtls plns = G.WindowT (BaseModel dtls plns) R.ReactMl () --FIXME: use Maybe


type GizmoTypes p' acts dtls plns cmds =
                    '[ MkPlan acts p'
                     , MkComponentListener plns
                     , MkComponentProperty dtls plns
                     , Gadget acts dtls plns cmds
                     ]

type Gizmo a' p' c' acts dtls plns cmds = Many (GizmoTypes p' acts dtls plns cmds)

-- type Widget pln acts dtls plns cmds = Many (Window dtls plns ': GizmoTypes pln acts dtls plns cmds)

-- TODO: Make into class
-- combine'
--     :: forall pln plns' acts dtls plns cmds.
--        Widget (Many plns') acts dtls plns cmds
--     -> Gizmo pln acts dtls plns cmds
--     -> Widget (Many (pln ': plns')) acts dtls plns cmds
-- combine' x y = x
--     & item' @(MkPlan acts (Many plns')) %~ (\mkPlans -> (./) <$> (y ^. (item @(MkPlan acts pln))) <*> mkPlans)
--     & item @(MkComponentListener plns) %~ (<> (y ^. (item @(MkComponentListener plns))))
--     & item @(MkComponentProperty dtls plns) %~ (<> (y ^. (item @(MkComponentProperty dtls plns))))
--     & item @(Gadget acts dtls plns cmds) %~ (<|> (y ^. (item @(Gadget acts dtls plns cmds))))

-- class Attach

combine
    :: forall a a' p p' c c' acts dtls plns cmds.
       Gizmo a p c acts dtls plns cmds
    -> Gizmo a' p' c' acts dtls plns cmds
    -> Gizmo (Append a a') (Append p p') (Append c c') acts dtls plns cmds
combine x y = x
    & item' @(MkPlan acts p) %~ (\mkPlans -> (/./) <$> mkPlans <*> y ^. (item @(MkPlan acts p')))
    & item @(MkComponentListener plns) %~ (<> (y ^. (item @(MkComponentListener plns))))
    & item @(MkComponentProperty dtls plns) %~ (<> (y ^. (item @(MkComponentProperty dtls plns))))
    & item @(Gadget acts dtls plns cmds) %~ (<|> (y ^. (item @(Gadget acts dtls plns cmds))))

blank :: Gizmo '[] '[] '[] acts dtls plns cmds
blank = pure nil
    ./ mempty
    ./ pure mempty
    ./ empty
    ./ nil

-- -- | Convert a Widget (which has a Window) back into a Gizmo, by wrapping it around a Component.
-- wack :: forall plns' acts dtls cmds. Widget (Many plns') acts dtls plns' cmds -> Many dtls -> F (R.Maker (Which acts)) (BaseEntity dtls plns')

--      -- Gizmo (BaseEntity dtls plns') acts d p cmds
-- wack w d = mkBaseEntity d mkPlns windw
--   where
--     mkPlns = w ^. (item @(MkPlan acts (Many plns')))
--     windw = w ^. (item @(Window dtls plns'))

----------------------------------------------------------

componentWindow
    :: D.DList ComponentListener -> D.DList ComponentProperty -> G.WindowT (BaseModel dtls plns) R.ReactMl ()
componentWindow ls ps = do
    s <- ask
    lift $
        R.lf
            (s ^. componentPlan  . component . to JE.toJS')
            ((D.fromList [ ("key", s ^. componentPlan . key . to JE.toJS')
                                     -- NB. render is not a 'R.Handle' as it returns an 'IO JSVal'
                                     , ("render", s ^. componentPlan . onRender . to JE.toJS')
                                     ]) <> (coerce ps))
            (coerce ls)

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
