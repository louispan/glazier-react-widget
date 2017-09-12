{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype where
 -- ( Archetype(..)
 -- , Archetyper
 -- , mkBasicEntity
 -- , implement
 -- , implement'
 -- , implant
 -- -- , tweakAction
 -- -- , tweakCommand
 -- -- , tweakSpecification
 -- -- , tweakSpecification'
 -- -- , tweakRequirement
 -- -- , tweakRequirement'
 -- ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Semigroup
import Control.Monad.Morph
import Data.Kind
import Data.Proxy
import qualified Data.Map.Strict as M
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE
import Control.DeepSeq
import qualified GHCJS.Types as J

newtype Archetype r s (a :: [Type]) acts (c :: [Type]) cmds =
    Archetype
    ( Proxy a
    , Proxy c
    , s -> R.ReactMlT STM ()
    , s -> STM r
    , F.Executor' cmds
    -> F.Handler' s acts cmds
    -> r
    -> F R.Reactor (TVar s))

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
implement
    :: ( NFData (Which acts')
       , Reinterpret' (Complement ta ha) acts'
       , Reinterpret' (Complement ba ha) acts'
       , Diversify acts (Complement ta ha)
       , Diversify acts (Complement ba ha)
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Prototype r r s s ba ta ha acts' bc hc cmds
    -> Archetype (Many r') (F.Design s) (Complement (AppendUnique ba ta) ha) acts (AppendUnique bc hc) cmds
implement (F.Prototype (bldr, disp, ts, hdl)) = Archetype
  ( Proxy
  , Proxy
  , F.componentWindow
  , fromEntity bldr
  , mkEntity bldr disp ts hdl
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
    -> F.Prototype '[r] reqs '[TVar s] specs a '[] '[] acts c '[] cmds
implant (Archetype (_, _, rnd, frmEnt, mkEnt)) = F.Prototype
    ( F.Builder (Proxy, Proxy, fromSpec, mkSpec)
    , F.divIfNeeded disp
    , mempty
    , mempty)
  where
    -- toBuilderHdl
    --   :: F.Handler' (F.Design specs) acts cmds
    --      -> TVar s
    --      -> Which acts
    --      -> MaybeT STM (DL.DList (Which cmds))
    -- toBuilderHdl hdl v l a = undefined -- FIXME: NOT possible!
    toBuilderHdl hdl v a = undefined -- FIXME: NOT possible!
    mkSpec exec hdl rs = do
        let r = fetch rs
        single <$> mkEnt exec (toBuilderHdl hdl) r
    fromSpec ss = do
        let s = fetch ss
        single <$> F.viewingTVar s id frmEnt
    disp d = do
        let s = d ^. (F.specifications . item)
        F.viewingTVar' s id rnd
        -- s' <- lift $ readTVar s
        -- rnd s'

mkEntity
    :: forall r r' s ba ta ha acts' acts bc hc cmds.
       ( NFData (Which acts')
       , Reinterpret' (Complement ta ha) acts'
       , Reinterpret' (Complement ba ha) acts'
       , Diversify acts (Complement ta ha)
       , Diversify acts (Complement ba ha)
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Builder r r s s ba acts' bc cmds
    -> F.Display s
    -> F.Triggers ta acts'
    -> F.Handler (F.Design s) ha acts' hc cmds -- handler for some of the triggers or builder
    -> F.Executor' cmds
    -> F.Handler' (F.Design s) acts cmds -- handler for builder and remaining triggers
    -> Many r'
    -> F R.Reactor (TVar (F.Design s))
mkEntity (F.Builder (_, _, _, mkSpec)) disp ts (F.Handler (_, _, internalHdl)) exec externalHdl rs = do
        let (ps, xs) = viewf rs
        ss <- mkSpec exec builderHdls xs -- externalToBuilderHdl xs
        F.mkDesign ps ss exec dgs' w
  where
    -- any actions required by ta, but not handled by ha, must be handled by externally provided handler
    -- given acts', use externalHdl which uses acts
    externalToInternalHdl v a = case reinterpret' @(Complement ta ha) @acts' a of
        Nothing -> empty
        Just a' -> externalHdl v (diversify @acts a')
    -- any actions required by ba, but not handled by ha, must be handled by externally provided handler
    -- given acts', use externalHdl which uses acts
    -- externalToBuilderHdl v l a = externalHdl v l (diversify @acts a)
    externalToBuilderHdl v a = case reinterpret' @(Complement ba ha) @acts' a of
        Nothing -> empty
        Just a' -> externalHdl v (diversify @acts a')
    builderHdls v a = internalHdl v a <|> externalToBuilderHdl v a
    internalHdls v a = internalHdl v a <|> externalToInternalHdl v a
    -- internalDelegate = toDelegate internalHdls exec
    dgs = toDelegates (F.getTriggers ts) internalHdls
    -- combine the callbacks with the same trigger key
    dgs' = M.toList . M.fromListWith (liftA2 (>>)) $ dgs
    w = F.renderDisplay (F.widgetDisplay <> disp)

toDelegates
    :: NFData (Which a)
    => DL.DList (F.Trigger' (Which a))
    -> F.Handler' s a c
    -> [(J.JSString, TVar s -> J.JSVal -> MaybeT IO (DL.DList (Which c)))]
toDelegates ts hdl = go <$> DL.toList ts
  where
    go (evt, t) = (evt, R.handleEventM t . hdl')
    hdl' v a = hoist atomically (hdl v a)

fromEntity :: (UniqueMember [JE.Property] r', r' ~ ([JE.Property] ': r))
    => F.Builder r r s s ba acts bc cmds -> F.Design s -> STM (Many r')
fromEntity (F.Builder (_, _, fromSpec, _)) d = do
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
