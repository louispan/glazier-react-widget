{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Core.Model where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import Control.Monad.RWS
import Data.Diverse.Lens
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

newtype PlanId = PlanId { unPlanId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)


-- | Interactivity for a particular DOM element.
data Gadget = Gadget
    { targetRef :: Maybe EventTarget
    , listeners :: DL.DList Listener
    } deriving (G.Generic)

makeLenses_ ''Gadget

newGadget :: Gadget
newGadget = Gadget Nothing mempty

data ShimListeners = ShimListeners
    -- render function of the ReactComponent
    { onRender :: J.Callback (IO J.JSVal)
    -- Run the on updated handlers below
    , onUpdated :: J.Callback (IO ())
    -- updates the componenRef
    , onRef :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeLenses_ ''ShimListeners

-- | Interactivity data for a react component
data Plan x = Plan
    -- a react "ref" to the javascript instance of ReactComponent
    -- so that react "componentRef.setState()" can be called.
    { componentRef :: Maybe ComponentRef
    , shimListeners :: Maybe ShimListeners
    -- This is the previous "react state"
    , previousFrameNum :: Int
    -- This the current "react state".
    -- The glazier framework engine will use this to send a render request
    -- if the current didn't match previous.
    , currentFrameNum :: Int
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
    -- interactivity data for child DOM elements
    , gadgets :: M.Map GadgetId Gadget
    -- interactivity data for child react components
    , plans :: M.Map PlanId (Plan x)
    } deriving (G.Generic)

makeLenses_ ''Plan

newPlan :: Plan x
newPlan = Plan
    Nothing
    Nothing
    0
    0
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty


-- | A 'Scene' contains interactivity data for all widgets as well as the model data.
data Scene x s =  Scene
    -- commands could be in a writer monad, but then you can't get
    -- a MonadWriter with ContT, but you can have a MonadState with ContT.
    { commands :: DL.DList x
    , plan :: Plan x
    , model :: s
    } deriving (G.Generic)

_commands :: Lens' (Scene x s) (DL.DList x)
_commands = lens commands (\s a -> s { commands = a})

_plan :: Lens' (Scene x s) (Plan x)
_plan = lens plan (\s a -> s { plan = a})

_model :: Lens (Scene x s) (Scene x s') s s'
_model = lens model (\s a -> s { model = a})

-- class HasScene c x s | c -> x s where
--     _scene :: Lens' c (Scene x s)

-- instance HasScene (Scene x s) x s where
--     _scene = id

-- data Frame x w =  Frame
--     -- commands could be in a writer monad, but then you can't get
--     -- a MonadWriter with ContT, but you can have a MonadState with ContT.
--     { commands :: DL.DList x
--     , world :: w
--     } deriving (G.Generic)

-- _world :: Lens' (Frame x s) s
-- _world = lens world (\s a -> s { world = a})

-- editWorld :: Lens' s' s -> Lens' (Scene x s') (Scene x s)
-- editWorld l = lens
--     (\s' -> s' & _model %~ (view l))
--     (\s' a -> a & _model %~ (\s -> (model s') & l .~ s))

editScene :: Functor f => LensLike' f s a -> LensLike' f (Scene x s) (Scene x a)
editScene l safa s = (\s' -> s { model = s'} ) <$> l afa' (model s)
  where
    afa' a = model <$> safa (s { model = a})

----------------------------------------------------------------------------------

-- type MonadWidget x s m = (MonadState (Scene x s) m, MonadWriter (DL.DList x) m)

mkId :: MonadState Int t => J.JSString -> t J.JSString
mkId n = do
    i <- get
    let i' = (+ 1) . (`mod` JE.maxSafeInteger) $ i
    put i'
    pure . J.append n . J.cons ':' . J.pack $ show i'

-- -- type Initializer x s m = (Widget x s m, MonadCont m)

-- -- | Create a Plan and Gadget for this widget if it did not already exists
-- -- FIXME: naming
-- initialize :: MonadWidget x s m => m ()
-- initialize = do
--     (pid, gid) <- ask
--     -- first check if plan exists
--     _plans.at pid %= (Just . initGadget gid . fromMaybe newPlan)
--   where
--     initGadget gid = _gadgets.at gid %~ (Just . fromMaybe newGadget)

----------------------------------------------------------------------------------

-- | post a command to be interpreted at the end of the frame
post :: (MonadState (Scene x s) m) => DL.DList x -> m ()
post xs = _commands %= (<> xs)

post1 :: (MonadState (Scene x s) m) => x -> m ()
post1 x = _commands %= (`DL.snoc` x)

post1' :: (AsFacet c x, MonadState (Scene x s) m) => c -> m ()
post1' = post1 . review facet



-- -- Add an action to run once after the next render
-- addOnceOnUpdated :: MonadState (Scene x s) m => x -> m ()
-- addOnceOnUpdated x = _plan._onceOnUpdated %= (`DL.snoc` x)

-- -- Add an action to run after every render
-- addEveryOnUpdated :: MonadState (Scene x s) m => x -> m ()
-- addEveryOnUpdated x = _plan._everyOnUpdated %= (`DL.snoc` x)
