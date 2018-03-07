{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Trigger where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Trans
import qualified Data.DList as DL
import Data.Maybe
import Data.Semigroup
import qualified GHCJS.Types as J
import Glazier.Core
import Glazier.React
import Glazier.React.Framework.Core.Display
import Glazier.React.Framework.Core.Model
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- | A simplified form of 'trigger' where all event info is dropped
-- and the given value is fired
trigger' :: (MonadReactor m)
    => GadgetId
    -> J.JSString
    -> b
    -> Delegate (Scene p m s) m b
trigger' gid n b = trigger gid n (const $ pure ()) (const b)

-- | Create callback for 'Notice' and add it to this state's dlist of listeners.
trigger ::
    ( MonadReactor m
    , NFData a
    )
    => GadgetId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> b)
    -> Delegate (Scene p m s) m b
trigger gid n goStrict = mkListener gid n goStrict'
  where
    goStrict' e = case JE.fromJSR e of
        Nothing -> pure Nothing
        Just e' -> Just <$> goStrict e'

-- | Create callbacks and add it to this state's dlist of listeners.
-- NB. You probably want ot use 'trigger' instead since most React callbacks
-- generate a 'Notice'.
-- Only the "ref" callback generate 'EventTarget' in which case you would want
-- to use 'withRef' instead.
mkListener ::
    ( MonadReactor m
    , NFData a
    )
    => GadgetId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> b)
    -> Delegate (Scene p m s) m b
mkListener gid n goStrict goLazy = delegate'' $ \this@(Obj{..}) fire -> do
    let goLazy' ma = case ma of
            Nothing -> pure ()
            Just a -> fire (goLazy a) *> rerender this
    (ds, cb) <- doMkCallback goStrict goLazy'
    doModifyIORef' self $ \me ->
        me & my._plan._listeners.at gid %~ (\ls -> Just $ (n, cb) `DL.cons` (fromMaybe DL.empty ls))
            & my._plan._disposeOnRemoved %~ (<> ds)

-- -- | feed the result from an Initializer into a handler, from left to right.
-- -- A simply way to think of the types is:
-- -- @
-- -- handledBy :: Initializer m s a -> Handler m s a b -> Initializer m s b
-- -- @
-- handledBy :: (Applicative f, Monad m) => f (m a) -> f (a -> m b) -> f (m b)
-- handledBy ini hdl = liftA2 (>>=) ini hdl
-- infixl 1 `handledBy` -- like >>=

-- -- | feed the result from an Initializer into a handler, from right to left.
-- handles :: Handler m s a b -> Initializer m s a -> Initializer m s b
-- handles = flip handledBy
-- infixr 1 `handles` -- like =<<

-- -- | feed as much of the result from an Initializer into a handler,
-- -- from left to right.
-- handledBy' :: forall m s a1 a2 b2 b3.
--     (Pretend a2 a1 b2 b3)
--     => Initializer m s (Which a1)
--     -> Handler m s (Which a2) (Which b2)
--     -> Initializer m s (Which b3)
-- handledBy' ini hdl = ini `handledBy` (pretend @a1 hdl)
-- infixl 1 `handledBy'` -- like >>=

-- -- | feed as much of the result from an Initializer into a handler,
-- -- from right to left.
-- handles' ::
--     (Pretend a2 a1 b2 b3)
--     => Handler m s (Which a2) (Which b2)
--     -> Initializer m s (Which a1)
--     -> Initializer m s (Which b3)
-- handles' = flip handledBy'
-- infixr 1 `handles'` -- like =<<

-- | This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an EventTarget
-- in the plan
withRef ::
    ( MonadReactor m
    )
    => GadgetId
    -> Delegate (Scene p m s) m ()
withRef i = mkListener i "ref" (pure . JE.fromJSR) id
    >>= hdlRef
  where
    -- hdlRef :: SceneHandler p s m (EventTarget) (Which '[])
    hdlRef j = delegate' $ \(Obj{..}) ->
        lift $ doModifyIORef' self (my._plan._refs.at i .~ Just j)

-- -- | Convert the original ContT to a ContT that
-- -- doens't call it's continuation, by 'const'ing the original contination
-- -- to 'pure'.
-- -- This is useful for converting a @Handler m s a ()@ to a @Handler m s a (Which '[])@
-- -- for combining using 'orHandler'.
-- -- This ContT can be run with  'Data.Diverse.Which.impossible'.
-- -- NB. This stops further ContT from executing.
-- terminate' :: Applicative m => ContT () m () -> ContT () m (Which '[])
-- terminate' = TE.terminate @(Which '[])
