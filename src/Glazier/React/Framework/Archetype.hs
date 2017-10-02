-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype where

import Control.Applicative
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

newtype Archetype r s (a :: [Type]) acts (c :: [Type]) cmds =
    Archetype
    ( Proxy a
    , Proxy c
    , s -> R.ReactMlT STM ()
    , s -> STM r
    , r -> STM s -- mkInactiveEntity
    , F.Activator' s acts cmds -- activator
    -- FIXME: Need expose handler? How?
    )

-- | Create a Prototype from an Archetype.
-- This wraps the specifications and requirements in an additional layer of 'Many'.
-- This is NOT the opposite of 'implement', that is:
--
-- @
-- implant . implement /= id
-- @
implant
    :: ( UniqueMember r reqs, UniqueMember (TVar s) specs)
    => Archetype r s a acts c cmds
    -> F.Prototype '[r] reqs '[TVar s] specs a '[] acts c cmds
implant (Archetype (_, _, rnd, frmEnt, mkEnt, activateEnt)) = F.prototyping
    (F.divIfNeeded disp)
    (F.Builder (fromSpec, mkSpec))
    (F.Activator (Proxy, Proxy, activateDesign))
    F.boring
    F.ignore
  where
    toBuilderHdl
      :: F.Handler' (F.Design specs) acts cmds
         -> TVar (F.Design specs)
         -> F.Handler' s acts cmds
    toBuilderHdl hdl' v _ = hdl' v
    activateDesign exec hdl' v = do
        d <- R.doSTM $ readTVar v
        let s = d ^.  (F.specifications . item)
        activateEnt exec (toBuilderHdl hdl' v) s
    mkSpec rs = do
        let r = fetch rs
        e <- mkEnt r
        single <$> newTVar e
    fromSpec ss = do
        let s = fetch ss
        single <$> (readTVar s >>= frmEnt)
    disp d = let s = d ^. (F.specifications . item)
             in lift (readTVar s) >>= rnd

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
implement
    :: ( NFData (Which acts')
       , Reinterpret' (Complement a h) acts'
       , Diversify acts (Complement a h)
       , r' ~ ([JE.Property] ': r))
    => F.Prototype r r s s a h acts' c cmds
    -> Archetype (Many r') (F.Design s) (Complement a h) acts c cmds
implement (F.Prototype (disp, bldr, activtr, ts, hdl)) = Archetype
  ( Proxy
  , Proxy
  , F.componentWindow
  , fromEntity bldr
  , mkInactiveEntity bldr
  , activateEntity activtr disp ts hdl
  )

mkInactiveEntity
    :: forall r r' s.
       ( r' ~ ([JE.Property] ': r))
    => F.Builder r r s s
    -> Many r'
    -> STM (F.Design s)
mkInactiveEntity (F.Builder (_, mkSpec)) rs = do
    let (ps, xs) = viewf rs
    ss <- mkSpec xs -- externalToBuilderHdl xs
    pure $ F.inactiveDesign ps ss

activateEntity
    :: forall s a h acts' acts c cmds.
       ( NFData (Which acts')
       , Reinterpret' (Complement a h) acts'
       , Diversify acts (Complement a h))
    => F.Activator (F.Design s) a acts' c cmds
    -> F.Display s
    -> F.Triggers a acts'
    -> F.Handler (F.Design s) h acts' c cmds -- handler for some of the triggers or builder
    -> F.Executor' cmds
    -> F.Handler' (F.Design s) acts cmds -- externally provided handler for builder and remaining triggers
    -> TVar (F.Design s)
    -> MaybeT (F R.Reactor) (STM ())
activateEntity (F.Activator (_, _, activateDesign)) disp ts (F.Handler (_, _, internalHdl)) exec externalHdl v
    = do
        d <- F.initDesign w v
        ls <- lift $ F.mkListeners exec delegates'
        let d' = d & F.plan . F.listeners %~ (<> ls)
        x <- activateDesign exec internalHdls v
        -- only write if activation succeeds
        pure (x >> writeTVar v d')
  where
    -- any actions required by @a@, but not handled by h, must be handled by externally provided handler
    -- NB. The type of Which is changed: given acts', use externalHdl which uses acts
    externalToInternalHdl :: TVar (F.Design s) -> Which acts' -> MaybeT STM (DL.DList (Which cmds))
    externalToInternalHdl v' a = case reinterpret' @(Complement a h) @acts' a of
        Nothing -> empty
        Just a' -> externalHdl v' (diversify @acts a')
    internalHdls v' a = internalHdl v' a <|> externalToInternalHdl v' a
    delegates = toDelegates (F.getTriggers ts) internalHdls v
    -- combine the callbacks with the same trigger key
    delegates' = M.toList . M.fromListWith (liftA2 (>>)) $ delegates
    w = F.renderDisplay (F.widgetDisplay <> disp)

toDelegates
    :: NFData (Which a)
    => DL.DList (F.Trigger' (Which a))
    -> F.Handler' s a c
    -> TVar s
    -> [(J.JSString, J.JSVal -> MaybeT IO (DL.DList (Which c)))]
toDelegates ts hdl v = go <$> DL.toList ts
  where
    go (evt, t) = (evt, R.handleEventM t hdl')
    hdl' a = hoist atomically (hdl v a)

fromEntity :: (r' ~ ([JE.Property] ': r))
    => F.Builder r r s s -> F.Design s -> STM (Many r')
fromEntity (F.Builder (fromSpec, _)) d = do
    let ps = d ^. F.properties
        ss = d ^. F.specifications
    rs <- fromSpec ss
    pure (ps ./ rs)

-- mkBasicEntity :: (r -> F R.Reactor s) -> r -> F R.Reactor (TMVar s)
-- mkBasicEntity mkEnt r = do
--     v <- R.doSTM newEmptyTMVar
--     s <- mkEnt v (Lens id) r
--     R.doSTM $ putTMVar v s
--     pure v

-- toDelegate
--     :: F.Handler' s a c
--     -> F.Executor' c
--     -> TMVar v
--     -> ReifiedLens' v s -- reified because output doesn't have lens type variables
--     -> Which a
--     -> MaybeT IO ()
-- toDelegate hdl exec v l a = hoist atomically (hdl v l a) >>= exec

-- tweakAction :: Prism' (Which a') (Which a) -> Archetyper v r s a c -> Archetyper v r s a' c
-- tweakAction l (Archetyper (mkEnt, frmEnt, dszpEnt)) =
--     Archetyper (\hdl -> mkEnt (\v l' -> hdl v l' . review l), frmEnt, dspEnt)

-- tweakCommand :: Iso' (Which c') (Which c) -> Archetyper v r s a c -> Archetyper v r s a c'
-- tweakCommand l (Archetyper (mkEnt, frmEnt, dspEnt)) =
--     Archetyper (\hdl exec -> mkEnt (\v l' -> fmap (view l) . hdl v l') (exec . review l), frmEnt, dspEnt)

-- tweakSpecification :: Iso' s' s -> Archetype v r s -> Archetype v r s'
-- tweakSpecification l (Archetype (mkEnt, frmEnt, disp)) =
--     Archetype ( \v (Lens l') r -> review l <$> mkEnt v (Lens (l' . l)) r
--               , frmEnt . view l
--               , magnify l disp)

-- tweakSpecification' :: Iso' s' s -> Archetyper v r s a c -> Archetyper v r s' a c
-- tweakSpecification' l f hdl exec = tweakSpecification l (f (F.magnifyHandler' (from l) hdl) exec)

-- tweakRequirement :: Iso' r' r -> Archetype v r s -> Archetype v r' s
-- tweakRequirement l (Archetype (mkEnt, frmEnt, disp)) =
--     Archetype ( \v l' r -> mkEnt v l' (view l r)
--               , fmap (review l) . frmEnt
--               , disp)

-- tweakRequirement' :: Iso' r' r -> Archetyper v r s a c -> Archetyper v r' s a c
-- tweakRequirement' l f hdl exec = tweakRequirement l (f hdl exec)
