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
newtype Shared i v = Shared
    { runShared :: (i, ReifiedLens' v i, TMVar v)
    }

_Shared :: Iso
    (Shared i v)
    (Shared i' v')
    (i, ReifiedLens' v i, TMVar v)
    (i', ReifiedLens' v' i', TMVar v')
_Shared = iso runShared Shared

instance R.Dispose i => R.Dispose (Shared i v) where
    dispose (Shared (i, _, _)) = R.dispose i

-- | (tm)utable (var)iable
tmvar :: Lens' (Shared i v) (TMVar v)
tmvar = _Shared . _3

-- | (i)mmutable (val)ue
ival :: Lens' (Shared i v) i
ival = _Shared . _1

-- | (tm)var to (i)mmutable (l)ens
tmil :: Lens' (Shared i v) (ReifiedLens' v i)
tmil = _Shared . _2

----------------------------------------------------------

data ComponentCommand
    = forall i v. RenderCommand (Shared i v) [JE.Property] J.JSVal
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

newtype Prototype (dtls :: [Type]) (plns :: [Type]) = Prototype
    { runPrototype ::
        ( Many dtls
        , Many plns
        , ComponentPlan
        )
    }
    deriving G.Generic

instance (R.Dispose (Many dtls), R.Dispose (Many plns)) => R.Dispose (Prototype dtls plns)

instance HasDetails (Prototype dtls plns) dtls where
    details = _Prototype . _1

instance HasPlans (Prototype dtls plns) plns where
    plans = _Prototype . _2

instance HasComponentPlan (Prototype dtls plns) where
    componentPlan = _Prototype . _3

_Prototype :: Iso
    (Prototype dtls plns)
    (Prototype dtls' plns')
    (Many dtls, Many plns, ComponentPlan)
    (Many dtls', Many plns', ComponentPlan)
_Prototype = iso runPrototype Prototype

----------------------------------------------------------

type Entity dtls plns v = Shared (Prototype dtls plns) v
type Entity' dtls plns = Shared (Prototype dtls plns) (Prototype dtls plns)

instance HasDetails (Entity dtls plns v) dtls where
    details = ival . details

instance HasPlans (Entity dtls plns v) plns where
    plans = ival . plans

instance HasComponentPlan (Entity dtls plns v) where
    componentPlan = ival . componentPlan

----------------------------------------------------------

componentGadget :: G.Gadget ComponentAction (Entity dtls plns v) (D.DList ComponentCommand)
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

componentWindow :: G.WindowT (Prototype dtls plns) R.ReactMl ()
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
type ToWindowProperties dtls plns = Prototype dtls plns -> D.DList WindowProperty

newtype WindowListener = WindowListener { runWindowListener :: R.Listener }
type ToWindowListeners dtls plns = Prototype dtls plns -> D.DList WindowListener

newtype ComponentListener = ComponentListener { runComponentListener :: R.Listener }
type ToComponentListeners plns = Many plns -> D.DList ComponentListener

type MkPlan plns acts = F (R.Maker (Which acts)) (Many plns)
type MkDetail dtls ols acts = Many ols -> F (R.Maker (Which acts)) (Many dtls)
type ToOutline ols dtls = Many dtls -> Many ols
type Device dtls plns v acts cmds = G.Gadget (Which acts) (Entity dtls plns v) (D.DList (Which cmds))

newtype Display dtls plns =
    Display ( Maybe (ToWindowProperties dtls plns)
            , Maybe (ToWindowListeners dtls plns)
            , Maybe ( Maybe (ToWindowProperties dtls plns)
                   -> Maybe (ToWindowListeners dtls plns)
                   -> G.WindowT (Prototype dtls plns) R.ReactMl ()))

instance Semigroup (Display dtls plns) where
    Display (ps, ls, Nothing) <> Display (ps', ls', w') =
        Display (ps <> ps', ls <> ls', w')
    Display (ps, ls, w) <> Display (ps', ls', Nothing) =
        Display (ps <> ps', ls <> ls', w)
    Display (ps, ls, Just w) <> Display (ps', ls', Just w') =
        Display
            ( Nothing
            , Nothing
            , Just (divWrapped (w ps ls <> w' ps' ls')))

-- | wrap with a div if there are properties and listeners
divWrapped
    :: G.WindowT (Prototype dtls plns) R.ReactMl ()
    -> Maybe (ToWindowProperties dtls plns)
    -> Maybe (ToWindowListeners dtls plns)
    -> G.WindowT (Prototype dtls plns) R.ReactMl ()
divWrapped w Nothing Nothing = w
divWrapped w p l = do
    s <- ask
    let l' = coerce $ fromMaybe mempty l s
        p' = coerce $ fromMaybe mempty p s
    lift . R.bh "div" p' l' $ G.runWindowT' w s

instance Monoid (Display dtls plns) where
    mempty = Display (Nothing, Nothing, Nothing)
    mappend = (<>)

renderDisplay :: Display dtls plns -> G.WindowT (Prototype dtls plns) R.ReactMl ()
renderDisplay (Display (p, l, w)) = fromMaybe mempty $ (\w' -> w' p l) <$> w

----------------------------------------------------------

newtype Gizmo o d p ols dtls plns v acts cmds = Gizmo (MkDetail d ols acts, ToOutline o dtls, MkPlan p acts, Device dtls plns v acts cmds)

-- | Wrap a widget into a component with it's own render and dispose functions
componentize
    :: forall dtls' plns' ols dtls plns v acts cmds.
    ( UniqueMember ComponentAction acts
    , UniqueMember ComponentCommand cmds
    , UniqueMember (Entity dtls plns v) dtls')
    => Display dtls plns
    -> Gizmo ols dtls plns ols dtls plns v acts cmds
    -> (Display dtls' plns', Gizmo ols '[Entity' dtls plns] '[] ols dtls' plns' v acts cmds)
componentize dsp (Gizmo (mkDtl, toOl, mkPln, dev)) =
    ( Display
        ( Nothing
        , Nothing
        , Just (divWrapped componentWindow))
    , Gizmo (mkDtl', toOl', pure nil, dev'))
  where
    w' = renderDisplay dsp
    mkDtl' o = do
        d <- mkDtl o
        single <$> mkEntity' d mkPln w'
    toOl' d = toOl (d ^. item @(Entity dtls plns v) . details)
    dev' = zoom (details . item @(Entity dtls plns v)) dev <|> componentGadget'
    componentGadget' = fmap pick <$> magnify (facet @ComponentAction) (zoom (details . item @(Entity dtls plns v)) componentGadget)

----------------------------------------------------------

-- | Make a Entity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkEntity'
    :: UniqueMember ComponentAction acts
    => Many dtls
    -> F (R.Maker (Which acts)) (Many plns)
    -> G.WindowT (Prototype dtls plns) R.ReactMl ()
    -> F (R.Maker (Which acts)) (Entity' dtls plns)
mkEntity' dtls mkPlns render = do
    frm <- R.mkEmptyFrame
    mdl <- (\plns compPln -> Prototype (dtls, plns, compPln)) <$> mkPlns <*> mkComponentPlan render frm
    R.putFrame frm mdl
    pure $ Shared (mdl, Lens id, frm)

----------------------------------------------------------

appendProxy :: Proxy a -> Proxy b -> Proxy (Append a b)
appendProxy _ _ = Proxy

type Widget o d p a c ols dtls plns v acts cmds = (Proxy a, Proxy c, Display dtls plns, Gizmo o d p ols dtls plns v acts cmds)

(+<>)
    :: Widget o d p a c ols dtls plns v acts cmds
    -> Widget o' d' p' a' c' ols dtls plns v acts cmds
    -> Widget (Append o o') (Append d d') (Append p p') (Append a a') (Append c c') ols dtls plns v acts cmds
(pa, pc, disp, Gizmo (mkDtl, toOl, mkPln, dev)) +<> (pa', pc', disp', Gizmo (mkDtl', toOl', mkPln', dev')) =
    ( appendProxy pa pa'
    , appendProxy pc pc'
    , disp <> disp'
    , Gizmo (\o -> (/./) <$> mkDtl o <*> mkDtl' o, undefined, undefined, undefined))
infixr 6 +<> -- like <>

-- infixl +<|> 3 -- like <|>






-- embedOutline :: Iso' (Many ols') (Many ols) -> Gizmo ols d p dtls plns v acts cmds -> Gizmo ols' d p dtls plns v acts cmds
-- embedOutline l (Gizmo (mkDtl, toOl, mkPln, dev)) = Gizmo (mkDtl . view l, review l . toOl, mkPln, dev)

-- embedPlan
--     :: forall p' plns' ols d p dtls v acts cmds.
--        UniqueMember p' plns'
--     => Iso' p' (Many p)
--     -> Gizmo ols d p dtls p v acts cmds
--     -> Gizmo ols d '[p'] dtls plns' v acts cmds
-- embedPlan l (Gizmo (mkDtl, toOl, mkPln, dev)) =
--     Gizmo
--         ( mkDtl
--         , toOl
--         , fmap (single . review l) mkPln
--         , zoom (embeddedPlanInEntity (item @p' . l)) dev)

-- embeddedPlanInPrototype :: Lens' (Many plns') (Many plns) -> Lens' (Prototype dtls plns') (Prototype dtls plns)
-- embeddedPlanInPrototype l = lens
--     (\(Prototype (d, p, c)) -> Prototype (d, p ^. l, c))
--     (\(Prototype (_, p, _)) (Prototype (d', p', c')) -> Prototype (d', (p & l .~ p'), c'))

-- embeddedPlanInEntity :: Lens' (Many plns') (Many plns) -> Lens' (Entity dtls plns' v) (Entity dtls plns v)
-- embeddedPlanInEntity i = lens
--     (\(Shared (x, Lens n, v)) -> Shared (x & (_Prototype . _2) .~ (x ^. (_Prototype . _2 . i)), Lens (n . embeddedPlanInPrototype i), v))
--     (\(Shared (x, Lens n, _)) (Shared (x', _, v')) -> Shared
--         ( x & embeddedPlanInPrototype i .~ x'
--         , Lens n
--         , v'))
