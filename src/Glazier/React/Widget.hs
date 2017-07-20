{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Widget where

import Control.Applicative
import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Coerce
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Kind
import Data.Proxy
import Data.Maybe
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

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

----------------------------------------------------------

data ComponentCommand
    = forall mdl. RenderCommand (Shared mdl) [JE.Property] J.JSVal
    | DisposeCommand (R.Disposable ())

data ComponentAction
    = ComponentRefAction J.JSVal
    | RenderAction
    | DisposeAction

-- | ComponentPlan has to be stored differently to other plans because mkComponentPlan needs
-- additional parameters
data ComponentPlan = ComponentPlan
    { _key :: J.JSString
    , _frameNum :: Int
    , _component :: R.ReactComponent
    , _componentRef :: J.JSVal
    , _deferredDisposables :: R.Disposable ()
    , _onRender ::  J.Callback (IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassy ''ComponentPlan

mkComponentPlan
    :: UniqueMember ComponentAction acts => G.WindowT mdl R.ReactMl () -> TMVar mdl -> F (R.Maker (Which acts)) ComponentPlan
mkComponentPlan render frm = R.hoistWithAction pick $ ComponentPlan
    <$> R.mkKey -- key
    <*> pure 0 -- frameNum
    <*> R.getComponent -- component
    <*> pure J.nullRef -- componentRef
    <*> pure mempty -- deferredDisposables
    <*> (R.mkRenderer render frm) -- onRender
    <*> (R.mkHandler $ pure . pure . ComponentRefAction) -- onComponentRef
    <*> (R.mkHandler $ pure . pure . const DisposeAction) -- onComponentDidUpdate

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

type BaseEntity dtls plns = Shared (BaseModel dtls plns)

instance HasDetails (BaseEntity dtls plns) dtls where
    details = ival . details

instance HasPlans (BaseEntity dtls plns) plns where
    plans = ival . plans

instance HasComponentPlan (BaseEntity dtls plns) where
    componentPlan = ival . componentPlan

----------------------------------------------------------

componentGadget :: G.Gadget ComponentAction (BaseEntity dtls plns) (D.DList ComponentCommand)
componentGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            (componentPlan . componentRef) .= node
            pure mempty

        RenderAction -> do
            -- Just change the state to a different number so the React PureComponent will call render()
            (componentPlan . frameNum) %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
            i <- JE.toJS <$> use (componentPlan . frameNum)
            r <- use (componentPlan . componentRef)
            s <- get
            pure . D.singleton $ RenderCommand s [("frameNum", JE.JSVar i)] r

        DisposeAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            ds <- use (componentPlan . deferredDisposables)
            (componentPlan . deferredDisposables) .= mempty
            pure . D.singleton . DisposeCommand $ ds

componentWindow :: G.WindowT (BaseModel dtls plns) R.ReactMl ()
componentWindow = do
    s <- ask
    lift $
        R.lf
            (s ^. componentPlan  . component . to JE.toJS')
            (D.fromList [ ("key", s ^. componentPlan . key . to JE.toJS')
                        -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
                        , ("render", s ^. componentPlan . onRender . to JE.toJS')
                        ])
            (D.fromList [ ("ref", s ^. componentPlan . onComponentRef)
                        , ("componentDidUpdate", s ^. componentPlan . onComponentDidUpdate)
                        ])


newtype WindowProperty = WindowProperty { runWindowProperty :: JE.Property }
type ToWindowProperties dtls plns = BaseModel dtls plns -> D.DList WindowProperty

newtype WindowListener = WindowListener { runWindowListener :: R.Listener }
type ToWindowListeners dtls plns = BaseModel dtls plns -> D.DList WindowListener

newtype ComponentListener = ComponentListener { runComponentListener :: R.Listener }
type ToComponentListeners plns = Many plns -> D.DList ComponentListener

type MkPlan plns acts = F (R.Maker (Which acts)) (Many plns)
type MkDetail ols dtls acts = Many ols -> F (R.Maker (Which acts)) (Many dtls)
type ToOutline ols dtls = Many dtls -> Many ols
type Device dtls plns acts cmds = G.Gadget (Which acts) (BaseEntity dtls plns) (D.DList (Which cmds))

newtype Display dtls plns =
    Display ( Maybe (ToWindowProperties dtls plns)
            , Maybe (ToWindowListeners dtls plns)
            , Maybe ( Maybe (ToWindowProperties dtls plns)
                   -> Maybe (ToWindowListeners dtls plns)
                   -> G.WindowT (BaseModel dtls plns) R.ReactMl ()))

instance Semigroup (Display dtls plns) where
    Display (ps, ls, Nothing) <> Display (ps', ls', w') =
        Display (ps <> ps', ls <> ls', w')
    Display (ps, ls, w) <> Display (ps', ls', Nothing) =
        Display (ps <> ps', ls <> ls', w)
    Display (ps, ls, Just w) <> Display (ps', ls', Just w') =
        Display
            ( Nothing
            , Nothing
            , Just (wrapWithDiv (w ps ls <> w' ps' ls')))

-- | wrap with a div if there are properties and listeners
wrapWithDiv
    :: G.WindowT (BaseModel dtls plns) R.ReactMl ()
    -> Maybe (ToWindowProperties dtls plns)
    -> Maybe (ToWindowListeners dtls plns)
    -> G.WindowT (BaseModel dtls plns) R.ReactMl ()
wrapWithDiv w Nothing Nothing = w
wrapWithDiv w p l = do
    s <- ask
    let l' = coerce $ fromMaybe mempty l s
        p' = coerce $ fromMaybe mempty p s
    lift . R.bh "div" p' l' $ G.runWindowT' w s

instance Monoid (Display dtls plns) where
    mempty = Display (Nothing, Nothing, Nothing)
    mappend = (<>)

renderDisplay :: Display dtls plns -> G.WindowT (BaseModel dtls plns) R.ReactMl ()
renderDisplay (Display (p, l, w)) = fromMaybe mempty $ (\w' -> w' p l) <$> w

----------------------------------------------------------

newtype Gizmo ols d p dtls plns acts cmds = Gizmo (MkDetail ols d acts, ToOutline ols d, MkPlan p acts, Device dtls plns acts cmds)

-- | Wrap a widget into a component with it's own render and dispose functions
componentize
    :: forall ols dtls plns cmds acts dtls' plns'.
    ( UniqueMember ComponentAction acts
    , UniqueMember ComponentCommand cmds
    , UniqueMember (BaseEntity dtls plns) dtls')
    => Display dtls plns
    -> Gizmo ols dtls plns dtls plns acts cmds
    -> (Display dtls' plns', Gizmo ols '[BaseEntity dtls plns] '[] dtls' plns' acts cmds)
componentize dsp (Gizmo (mkDtl, toOl, mkPln, dev)) =
    ( Display
        ( Nothing
        , Nothing
        , Just (wrapWithDiv componentWindow))
    , Gizmo (mkDtl', toOl', pure nil, dev'))
  where
    w' = renderDisplay dsp
    mkDtl' o = do
        d <- mkDtl o
        single <$> mkBaseEntity d mkPln w'
    toOl' d = toOl (d ^. item @(BaseEntity dtls plns) . details)
    dev' = zoom (details . item @(BaseEntity dtls plns)) dev <|> componentGadget'
    componentGadget' = (fmap pick) <$> magnify (facet @ComponentAction) (zoom (details . item @(BaseEntity dtls plns)) componentGadget)

----------------------------------------------------------

-- | Make a BaseEntity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkBaseEntity
    :: UniqueMember ComponentAction acts
    => Many dtls
    -> F (R.Maker (Which acts)) (Many plns)
    -> G.WindowT (BaseModel dtls plns) R.ReactMl ()
    -> F (R.Maker (Which acts)) (BaseEntity dtls plns)
mkBaseEntity dtls mkPlns render = do
    frm <- R.mkEmptyFrame
    mdl <- (\plns compPln -> BaseModel (dtls, plns, compPln)) <$> mkPlns <*> mkComponentPlan render frm
    R.putFrame frm mdl
    pure (Shared (frm, mdl))
