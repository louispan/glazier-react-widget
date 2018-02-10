{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Model where

-- import Control.Applicative
import qualified Control.Disposable as CD
import Control.Lens
import qualified Data.DList as DL
import Data.Generics.Product
import Data.Kind
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Obj as F
import qualified JavaScript.Extras as JE
----------------------------------------------------------

type family OnSpec (w :: Type -> Type) (s :: Type) = (r :: Type) | r -> w s

-- | Something that knows how to get and set (but not make) a spec
class ViaSpec (w :: Type -> Type) where
    -- | given a lens from @t@ to @s@,
    -- change something that knows how to manipulate an @s@
    -- to something that knows how to manipulate a @t@.
    viaSpec :: Lens' t s -> OnSpec w s -> OnSpec w t

type family OnInfo (w :: Type -> Type) (i :: Type) = (r :: Type) | r -> w i

-- | Something that knows how to get and set (but not make) a plan
class ViaInfo (w :: Type -> Type) where
    -- | given lens from @q@ to @p@,
    -- change something that knows how to manipulate an @p@
    -- to something that knows how to manipulate a @q@.
    viaInfo :: Lens' j i -> OnInfo w i -> OnInfo w j

-- | One for every archetype, may be shared for many prototypes
data Plan x m = Plan
    { component :: R.ReactComponent
    , key :: R.ReactKey
    , frameNum :: Int
    , disposeOnRemoved :: CD.Disposable -- things to dispose when this widget is removed
    , disposeOnUpdated :: CD.Disposable -- things to dispose on updated
    , doOnUpdated :: m (DL.DList x) -- additional monadic action to take after a rerender
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    } deriving (G.Generic)

type Frame x m s = (Plan x m, s)

plan :: Lens' (Frame x m s) (Plan x m)
plan = _1

model :: Lens' (Frame x m s) s
model = _2

-- | Mutable
type Scene x m v s = F.Obj v (Frame x m s)

mkPlan :: R.MonadReactor x m => J.JSString -> m (Plan x m)
mkPlan n = Plan
    <$> R.getComponent
    <*> R.mkReactKey n
    <*> pure 0
    <*> pure mempty -- ^ disposeOnRemoved
    <*> pure mempty -- ^ disposeOnUpdated
    <*> pure (pure mempty) -- ^ doOnUpdated
    <*> pure Nothing -- ^ callback
    <*> pure Nothing -- ^ render

rerender :: R.MonadReactor x m => Scene x m v s -> m ()
rerender (F.Obj ref its) = do
    obj <- R.doReadIORef ref
    let (i, obj') = obj & (its.plan.field @"frameNum") <%~ ((+ 1) . (`mod` JE.maxSafeInteger))
    R.doWriteIORef ref obj'
    R.setComponentState
        (JE.fromProperties [("frameNum", JE.toJS' i)])
        (obj ^. (its.plan.field @"component"))

-- -- | If a new item was added, then we need to delay focusing until after the next render
-- focus :: (R.MonadReactor x m)
--     => IORef v
--     -> Lens' v (Plan x m, s)
--     -> R.EventTarget
--     -> m ()
-- focus ref its j = do
--     R.doModifyIORef' ref (its.plan.field @"doOnUpdated" %~ (\g -> liftA2 const g (R.focus j)))
--     rerender ref its
