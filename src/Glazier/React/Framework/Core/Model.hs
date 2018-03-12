{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Core.Model where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.RWS
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import Glazier.React
import qualified JavaScript.Extras as JE


-- In order to remove effects we need to change to use State monad
-- where the state contains
-- ( the pure state s
-- , new triggers to activate: J.JSString, NOtice -> IO a, a -> b
-- , a way to retrieve io values back out of commands
-- )
-- and model needs to change so
-- component is map
-- react key is gadget key and is passed with Obj as the reader env
-- actually every is made into a map, so Plan is lifted into World = Map Plan


-- On the pure state processing side
-- create tmvar
-- define command that uses the tmvar
-- return command

-- This returns multiple commands split into multiple transactions
-- that can be run concurrently
-- a <- mkTMVar
-- b <- mkTMVar
-- cmda <- GetFilename (writeTMVar a)
-- cmdb <- GetUserName (writeTMVar b)
-- cmdc <- STMEffect $ do
--     obj <- ObjRef
--     do something with obj a + b
-- pure (new state, [cmda, cmdb, cmdc])
--
-- alternatively a single transaction run in serial.
-- let cmd = STMEffect $ (`runContT` pure) $ do
--   fn <- ContT $ doEffect GetFilename
--   un <- ContT $ doEffect GetUserName
--   do something with obj, fn and un
-- pure cmd
--
-- How to sugar conconurret version above so that you don't mkTMVar explicitly?
-- There is a difference in events and commands?
-- comands gets pushed to a separate queue (WriterT?)


-- So the engine (fired by trigger) now does:
-- read word IORef
-- run trigger state function and get (commands, new state)
-- update state ioref
-- check dirty and render
--
-- spawn multiple threads to asynchronously:
--   - run commands, retrying forever the ones that fail.
--   - command transactions should be small if possible



-- the base monad bind:
-- returns a STM async
-- which actually creates a STM Ref to fire once-off results into
-- for the pure thing to read?


-- This id can also be used as the react @key@
newtype GadgetId = GadgetId { unGadgetId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)

-- | Interactivity for a particular DOM element.
data Gadget = Gadget
    { reactRef :: JE.JSRep
    , listeners :: DL.DList Listener
    } deriving (G.Generic)

makeLenses_ ''Gadget

newtype PlanId = PlanId { unPlanId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)

mkPlanId :: TVar Int ->  J.JSString -> STM PlanId
mkPlanId i n = do
    i' <- readTVar i
    modifyTVar' i (\j -> (j `mod` JE.maxSafeInteger) + 1)
    pure . PlanId . J.append n . J.cons ':' . J.pack $ show i'

type WidgetId = (PlanId, GadgetId)

_planId :: Lens' WidgetId PlanId
_planId = _1

_gadgetId :: Lens' WidgetId GadgetId
_gadgetId = _2

-- | One for every archetype, may be shared for many prototypes
data Plan x = Plan
    -- the same react component may be shared amongs many widgets
    -- shared react component will all be rerendered at the same time
    { component :: ReactComponent
    -- This is the previous "react state"
    , previousFrameNum :: Int
    -- This the current "react state".
    -- The glazier framework engine will use this to send a render request
    -- if the current didn't match previous.
    , currentFrameNum :: Int
    -- render function of the ReactComponent
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    -- Run hte on updated handlers below
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
      -- Things to dispose when this widget is removed
      -- cannot be hidden inside afterOnUpdated, as this also needs to be used
      -- when finalizing
    , disposeOnRemoved :: CD.Disposable
    --  Things to dispose on updated
    , disposeOnUpdated :: CD.Disposable
    -- additional actions to take after every dirty
    , everyOnUpdated :: DL.DList x
     -- additional actions to take after a dirty
    , onceOnUpdated :: DL.DList x
    -- If the last gadget is removed, then this Plan should be removed
    , gadgets :: M.Map GadgetId Gadget
    } deriving (G.Generic)

makeLenses_ ''Plan

-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
type Scene x s =
    ( M.Map PlanId (Plan x)
    , s
    )

_plans :: Lens (Scene x s) (Scene x' s) (M.Map PlanId (Plan x)) (M.Map PlanId (Plan x'))
_plans = _1

_model :: Lens (Scene x s) (Scene x s') s s'
_model = _2

editScene :: Lens' s' s -> Lens' (Scene x s') (Scene x s)
editScene l = alongside id l

type Widget x s m = MonadRWS WidgetId (DL.DList x) (Scene x s) m

myPlan :: Widget x s m => m (ReifiedTraversal' (Scene x s) (Plan x))
myPlan = do
    pid <- view _planId
    pure (Traversal $ _plans.ix pid)

myGadget :: Widget x s m => m (ReifiedTraversal' (Scene x s) Gadget)
myGadget = do
    (pid, gid) <- ask
    pure (Traversal $ _plans.ix pid._gadgets.ix gid)

----------------------------------------------------------------------------------

-- Add an action to run once after the next render
addOnceOnUpdated :: Widget x s m => x -> m ()
addOnceOnUpdated x = do
    Traversal _pln <- myPlan
    _pln._onceOnUpdated %= (`DL.snoc` x)

-- Add an action to run after every render
addEveryOnUpdated :: Widget x s m => x -> m ()
addEveryOnUpdated x = do
    Traversal _pln <- myPlan
    _pln._everyOnUpdated %= (`DL.snoc` x)
