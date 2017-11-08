{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Core where
--   ( WidgetCommand(..)
--   , WidgetAction(..)
--   , HasProperties(..)
--   , HasPlan(..)
--   , HasSpecifications(..)
--   , HasPlans(..)
--   , Design(..)
--   , _Design
--   , Entity
--   , withTMVar
--   , inTMVar
--   , widgetGadget
--   , widgetWindow
--   , putEntity
--   ) where
-- -- We want to hide mkPlan

-- import Control.Applicative
-- import Control.DeepSeq
import Control.Lens
-- import Control.Monad.Morph
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.State.Strict
-- import Data.Diverse.Lens
-- import qualified Data.DList as DL
-- import Data.IORef
-- import qualified Data.JSString as JS
import Data.Kind
-- import Data.Semigroup
-- import Data.Tagged
import qualified GHC.Generics as G
-- import qualified GHCJS.Foreign.Callback as J
-- import qualified GHCJS.Types as J
import qualified Glazier.React as R
-- import qualified Glazier.React.Commands.Rerender as C
-- import qualified JavaScript.Extras as JE

----------------------------------------------------------

-- usingIORef :: R.MonadReactor m => IORef s -> MaybeT (StateT s m) a -> MaybeT m a
-- usingIORef this m = do
--     s <- lift $ R.doReadIORef this
--     (a, s') <- lift $ runStateT (runMaybeT m) s
--     case a of
--         Nothing -> empty
--         Just a' -> do
--             lift $ R.doWriteIORef this s'
--             pure a'

-- catMaybeT :: (Applicative m, Semigroup a) => MaybeT m a -> MaybeT m a -> MaybeT m a
-- catMaybeT (MaybeT m) (MaybeT m') = MaybeT (liftA2 (<>) m m')

----------------------------------------------------------

-- -- | This helps prevernt nested levels of 'diversify'.
-- -- Known types are tracked in @a'@ while the @a@ type is left polymorphic.
-- -- 'runWhichever' should be used to extract the final Which type, which ensures
-- -- @a'@ fulfills all the constraints of @a@.
-- type Whichever (a' :: [Type]) (a :: [Type]) = Tagged a' (Which a)

-- runWhichever :: Whichever a a -> Which a
-- runWhichever = unTagged

----------------------------------------------------------

-- | Plan has to be stored differently to other plans because mkPlan needs
-- additional parameters

newtype FrameNum = FrameNum { runFrameNum :: Int } deriving R.Dispose

type family Modeller (w :: Type -> Type) (s :: Type) = (r :: Type) | r -> w s

-- | Something that knows how to get and set (but not make) a model
class ViaModel (w :: Type -> Type) where
    -- | given a lens from @t@ to @s@,
    -- change something that knows how to manipulate an @s@
    -- to something that knows how to manipulate a @t@.
    viaModel :: Lens' t s -> Modeller w s -> Modeller w t

-- class IORefModel x y where
--     -- | Given something that knows how to manipulate or make an @s@
--     -- change it to something that can manipulate or make an @IORef s@
--     ioRefModel :: x -> y

type family Planner (w :: Type -> Type) (p :: Type) = (r :: Type) | r -> w p

-- | Something that knows how to get and set (but not make) a plan
class ViaPlan (w :: Type -> Type) where
    -- | given lens from @q@ to @p@,
    -- change something that knows how to manipulate an @p@
    -- to something that knows how to manipulate a @q@.
    viaPlan :: Lens' q p -> Planner w p -> Planner w q


-- FIXME: key, ioref s
data ComponentModel = ComponentModel
    { _component :: R.ReactComponent
    , _componentKey :: R.Key
    , _componentRender ::  R.Renderer
    , _componentListeners :: [R.Listener]
    } deriving (G.Generic)

makeClassy ''ComponentModel

instance R.Dispose ComponentModel


-- data ComponentPlan = ComponentPlan
--     { _component :: R.ReactComponent
--     , _onRender ::  J.Callback (IO J.JSVal)
--     , _onComponentRef :: J.Callback (J.JSVal -> IO ())
--     , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
--     } deriving (G.Generic)

-- makeClassy ''ComponentPlan

-- instance R.Dispose ComponentPlan

-- data Plan = Plan
--     { _frameNum :: FrameNum
--     , _componentRef :: C.ComponentRef
--     , _deferredDisposables :: R.Disposable ()
--     , _listeners :: [R.Listener]
--     , _key :: J.JSString
--     , _componentPlan' :: Maybe ComponentPlan
--     } deriving (G.Generic)

-- makeClassy ''Plan

-- instance R.Dispose Plan

----------------------------------------------------------

-- class HasSpecifications c specs | c -> specs where
--     specifications :: Lens' c (Many specs)

----------------------------------------------------------

-- newtype Design (specs :: [Type]) = Design
--     { runDesign ::
--         ( [JE.Property]
--         , Many specs
--         -- , Plan
--         )
--     }
--     deriving G.Generic

-- -- | UndecidableInstances, but safe because @Many specs@ is smaller than @Design specs@
-- instance (R.Dispose (Many specs)) => R.Dispose (Design specs)

-- instance HasPlan (Design specs) where
--     plan = _Design . _3

-- instance HasProperties (Design specs) where
--     properties = _Design . _1

-- instance HasSpecifications (Design specs) specs where
--     specifications = _Design . _2

-- _Design :: Iso
--     (Design specs)
--     (Design specs')
--     ([JE.Property], Many specs, Plan)
--     ([JE.Property], Many specs', Plan)
-- _Design = iso runDesign Design

----------------------------------------------------------

-- rerender :: Monad m => MaybeT (StateT (Design specs) m) C.Rerender
-- rerender = do
--     -- Just change the state to a different number so the React PureComponent will call render()
--     (plan . frameNum) %= (\(FrameNum i) -> FrameNum $ (i `mod` JE.maxSafeInteger) + 1)
--     FrameNum i <- use (plan . frameNum)
--     r <- use (plan . componentRef)
--     pure $ C.Rerender r [("frameNum", JE.JSVar $ JE.toJS i)]

----------------------------------------------------------

-- queueDisposable :: (Monad m, R.Dispose a) => a -> MaybeT (StateT (Design specs) m) ()
-- queueDisposable a = (plan . deferredDisposables) %= (>> R.dispose a)

-- inactivePlan
--     :: Plan
-- inactivePlan = Plan
--     (FrameNum 0) -- frameNum
--     (C.ComponentRef J.nullRef) -- componentRef
--     mempty -- deferredDisposables
--     mempty -- traverse go ts -- triggers
--     mempty -- R.mkKey' -- key
--     Nothing -- componentPlan

-- mkComponentPlan
--     :: R.MonadReactor m
--     => (Design specs -> R.ReactMlT m ())
--     -> IORef (Design specs)
--     -> m (ComponentPlan)
-- mkComponentPlan w this = ComponentPlan
--     <$> R.getComponent -- component
--     <*> R.mkRenderer rnd -- onRender
--     <*> (R.mkCallback (pure . \j -> [j]) (usingIORef this . zoom plan . doOnComponentRef) pure) -- onComponentRef
--     <*> (R.mkCallback (pure . const [()]) (
--             usingIORef this
--             . zoom plan
--             . const doOnComponentDidUpdate) (lift . R.runDisposable)) --onComopnentDidUpdate
--   where
--     rnd = lift (R.doReadIORef this) >>= w

--     doOnComponentRef :: Monad m => J.JSVal -> MaybeT (StateT Plan m) [()]
--     doOnComponentRef j = do
--         componentRef .= C.ComponentRef j
--         pure []

--     doOnComponentDidUpdate :: Monad m => MaybeT (StateT Plan m) [R.Disposable ()]
--     doOnComponentDidUpdate = do
--         -- Run delayed commands that need to wait until frame is re-rendered
--         -- Eg focusing after other rendering changes
--         ds <- use deferredDisposables
--         deferredDisposables .= mempty
--         pure [ds]

-- initPlan
--     :: R.MonadReactor m
--     => (Design specs -> R.ReactMlT m ())
--     -> IORef (Design specs)
--     -> Plan
--     -> m (Maybe Plan)
-- initPlan _ _ (Plan _ _ _ _ _ (Just _)) = pure Nothing
-- initPlan w this (Plan frm cRef defDisp ls k Nothing) = fmap Just $ Plan frm cRef defDisp ls
--     <$> ((k `JS.append`) <$> R.mkKey') -- key
--     <*> (Just <$> mkComponentPlan w this)

-- mkListeners
--     :: (R.MonadReactor m, NFData a)
--     => (a -> m (DL.DList (Which cmds)))
--     -> (Which cmds -> IO ())
--     -> [(J.JSString, J.JSVal -> IO (DL.DList a))]
--     -> m [R.Listener]
-- mkListeners hdl exec = traverse toListener -- triggers
--   where
--     toListener (n, f) = (\a -> (n, a)) <$> R.mkCallback f hdl exec

-- inactiveDesign
--     :: [JE.Property]
--     -> Many specs
--     -> Design specs
-- inactiveDesign ps specs = Design (ps, specs, inactivePlan)

-- initDesign
--     :: R.MonadReactor m => (Design specs -> R.ReactMlT m ())
--     -> IORef (Design specs)
--     -> MaybeT m (Design specs)
-- initDesign w this = do
--     obj <- lift $ R.doReadIORef this
--     pln <- MaybeT $ initPlan w this (obj ^. plan)
--     pure (obj & plan .~ pln)

-- componentWindow :: Monad m => Design specs -> R.ReactMlT m ()
-- componentWindow s =
--     let cPlan = s ^. plan . componentPlan'
--     in case cPlan of
--         Nothing -> pure ()
--         Just cPlan' -> R.lf
--            (cPlan' ^. component . to JE.toJS')
--             [ ("ref", cPlan' ^. onComponentRef)
--             , ("componentDidUpdate", cPlan' ^. onComponentDidUpdate)
--             ]
--             [ ("key", s ^. plan . key . to JE.toJS')
--             -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
--             , ("render", cPlan' ^. onRender . to JE.toJS')
--             ]
