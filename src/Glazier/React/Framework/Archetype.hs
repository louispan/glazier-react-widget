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
import Data.Maybe
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
import Data.Coerce

newtype Archetype v r s (a :: [Type]) acts (c :: [Type]) cmds =
    Archetype
    ( Proxy a
    , Proxy c
    , s -> R.ReactMlT STM ()
    , s -> STM r
    , F.Handler' v s acts cmds
    -> F.Executor' cmds
    -> TMVar v
    -> ReifiedLens' v s
    -> r
    -> F R.Reactor s)


-- newtype Wack = Wack Int

-- wack :: TVar Int -> TVar Wack
-- wack = coerce

mkBasicEntity :: (TMVar s -> ReifiedLens' s s -> r -> F R.Reactor s) -> r -> F R.Reactor (TMVar s)
mkBasicEntity mkEnt r = do
    v <- R.doSTM newEmptyTMVar
    s <- mkEnt v (Lens id) r
    R.doSTM $ putTMVar v s
    pure v

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
implement
    :: ( NFData (Which ta)
       , Reinterpret' (Complement ta h) ta
       , Diversify acts (Complement ta h)
       , Diversify acts ba
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Prototype v r r s s ba ba ta h ta bc tc cmds
    -> Archetype v (Many r') (F.Design s) (AppendUnique ba (Complement ta h)) acts (AppendUnique bc tc) cmds
implement (F.Prototype (bldr, disp, ts, hdl)) = Archetype
  ( Proxy
  , Proxy
  , F.componentWindow
  , fromEntity bldr
  , mkEntity bldr disp ts hdl
  )

-- -- | Create a Prototype from an Archetype.
-- -- This wraps the specifications and requirements in an additional layer of 'Many'.
-- -- This is NOT the opposite of 'implement', that is:
-- --
-- -- @
-- -- implant . implement /= id
-- -- @
-- implant
--     :: ( UniqueMember r reqs, UniqueMember (TMVar s) specs)
--     => Archetype s r s a acts c cmds
--     -> F.Prototype v '[r] reqs '[TMVar s] specs a acts '[] '[] acts' c '[] cmds
-- implant (Archetype (_, _, rnd, frmEnt, mkEnt)) = F.Prototype
--     ( F.Builder (Proxy, Proxy, fromSpec, mkSpec)
--     , F.divIfNeeded disp
--     , mempty
--     , mempty)
--   where
--     -- toBuilderHdl
--       -- :: F.Handler' v (F.Design specs) acts cmds
--       --    -> TMVar s
--       --    -> ReifiedLens' s s
--       --    -> Which acts
--       --    -> MaybeT STM (Which cmds)
--     -- toBuilderHdl hdl v l a = undefined -- NOT possible!
--     -- mkSpec = undefined
--     mkSpec hdl exec v (Lens l) rs = do
--         let r = fetch rs
--         single <$> mkEnt (toBuilderHdl hdl) exec v (Lens (l . F.specifications . item)) r
--     fromSpec ss = do
--         let s = fetch ss
--         s' <- readTMVar s
--         single <$> frmEnt s'
--     disp d = do
--         let s = d ^. (F.specifications . item)
--         s' <- lift $ readTMVar s
--         rnd s'

mkEntity
    :: forall v r r' s ba ta h acts bc tc cmds.
       ( NFData (Which ta)
       , Reinterpret' (Complement ta h) ta
       , Diversify acts (Complement ta h)
       , Diversify acts ba
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Builder v r r s s ba ba bc cmds
    -> F.Display s
    -> F.Triggers ta ta
    -> F.Handler v (F.Design s) h ta tc cmds -- handler for some of the triggers
    -> F.Handler' v (F.Design s) acts cmds -- handler for builder and remaining triggers
    -> F.Executor' cmds
    -> TMVar v
    -> ReifiedLens' v (F.Design s)
    -> Many r'
    -> F R.Reactor (F.Design s)
mkEntity (F.Builder (_, _, _, mkSpec)) disp ts (F.Handler (_, _, internalHdl)) externalHdl exec v l@(Lens l') rs = do
        let (ps, xs) = viewf rs
        ss <- mkSpec externalToBuilderHdl exec v l xs
        d <- F.mkDesign ps ss w cbs' v l'
        pure d
  where
    externalToInternalHdl v' l'' a = case reinterpret' @(Complement ta h) @ta a of
        Nothing -> empty
        Just a' -> externalHdl v' l'' (diversify @acts a')
    externalToBuilderHdl v' l'' a = externalHdl v' l'' (diversify @acts a)
    internalHdls v' l'' a = internalHdl v' l'' a <|> externalToInternalHdl v' l'' a
    internalDelegate = toDelegate internalHdls exec
    cbs = toCallbacks (F.getTriggers ts) internalDelegate
    -- combine the callbacks with the same trigger key
    cbs' v' l'' = M.toList . M.fromListWith (liftA2 (>>)) $ cbs v' l''
    w = F.renderDisplay (F.widgetDisplay <> disp)

fromEntity :: (UniqueMember [JE.Property] r', r' ~ ([JE.Property] ': r))
    => F.Builder v r r s s ba acts bc cmds -> F.Design s -> STM (Many r')
fromEntity (F.Builder (_, _, fromSpec, _)) d = do
    let ps = d ^. F.properties
        ss = d ^. F.specifications
    rs <- fromSpec ss
    pure (ps ./ rs)

toDelegate
    :: F.Handler' v s a c
    -> F.Executor' c
    -> TMVar v
    -> ReifiedLens' v s -- reified because output doesn't have lens type variables
    -> Which a
    -> MaybeT IO ()
toDelegate hdl exec v l a = hoist atomically (hdl v l a) >>= exec

toCallbacks
    :: NFData a
    => DL.DList (F.Trigger' a)
    -> (TMVar v -> ReifiedLens' v s -> a -> MaybeT IO ())
    -> TMVar v
    -> ReifiedLens' v s -- reified because output doesn't have lens type variables
    -> [(J.JSString, J.JSVal -> IO ())]
toCallbacks ts delegate v l = go <$> DL.toList ts
  where
    go (evt, t) = (evt, fmap (fromMaybe ()) . runMaybeT . R.handleEventM t (delegate v l))

-- tweakAction :: Prism' (Which a') (Which a) -> Archetyper v r s a c -> Archetyper v r s a' c
-- tweakAction l (Archetyper (mkEnt, frmEnt, dspEnt)) =
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
