{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype
 ( Archetype(..)
 , Archetyper
 , mkBasicEntity
 , commission
 , commission'
 , redraft
 , prismAction
 , isoCommand
 , isoSpecification
 , isoSpecification'
 , isoRequirement
 , isoRequirement'
 ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Semigroup
import Control.Monad.Morph
import Data.Maybe
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

newtype Archetype v r s = Archetype ( TMVar v -> ReifiedLens' v s -> r -> F R.Reactor s
                                  , s -> STM r
                                  , s -> R.ReactMlT STM ())

-- | Used where an externally given delegate is required to 'commission'' an 'Archetype'
type Archetyper v r s a c = F.Handler' v s a c -> F.Executor' c -> Archetype v r s

mkBasicEntity :: (TMVar s -> ReifiedLens' s s -> r -> F R.Reactor s) -> r -> F R.Reactor (TMVar s)
mkBasicEntity mkEnt r = do
    v <- R.doSTM newEmptyTMVar
    s <- mkEnt v (Lens id) r
    R.doSTM $ putTMVar v s
    pure v

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
commission
    :: ( NFData (Which a)
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Prototype v r r s s a a a c c c
    -> Archetype v (Many r') (F.Design s)
commission (F.Prototype (bldr, disp, ts, hdl, exec)) = doCommission bldr disp ts delegate
  where
    delegate = toDelegate (F.getHandler hdl) (F.getExecutor exec) -- internal handling

-- | A variation of 'commission' where the Prototype has incomplete handlers
-- and so required external handlers to complete the Archetype.
commission'
    :: ( Reinterpret' h a
       , NFData (Which a)
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Prototype v r r s s a (Complement a h) a c c c
    -> Archetyper v (Many r') (F.Design s) (Which h) c'
commission' (F.Prototype (bldr, disp, ts, hdl, exec)) hdl' exec' = doCommission bldr disp ts delegate
  where
    internalDelegate = toDelegate (F.getHandler hdl) (F.getExecutor exec) -- internal handling
    externalDelegate v l a = case reinterpret' a of
        Nothing -> empty
        Just a' -> toDelegate hdl' exec' v l a'
    delegate v l a = internalDelegate v l a <|> externalDelegate v l a

doCommission
    :: ( NFData (Which a)
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Builder v r r s s
    -> F.Display s
    -> F.Triggers a a
    -> (TMVar v
       -> ReifiedLens' v (F.Design s) -- reified because output doesn't have lens type variables
       -> Which a
       -> MaybeT IO ())
    -> Archetype v (Many r') (F.Design s)
doCommission (F.Builder (mkSpec, fromSpec)) disp ts delegate = Archetype (mkEnt, frmEnt, F.componentWindow)
  where
    cbs = toCallbacks (F.getTriggers ts) delegate
    cbs' v l = M.toList . M.fromListWith (liftA2 (>>)) $ (cbs v l)
    mkEnt v l@(Lens l') rs = do
        let (ps, xs) = viewf rs
        ss <- mkSpec v l xs
        d <- F.mkDesign ps ss w cbs' v l'
        pure d
    w = F.renderDisplay (F.widgetDisplay <> disp)
    frmEnt d = do
        let ps = d ^. F.properties
            ss = d ^. F.specifications
        rs <- fromSpec ss
        pure (ps ./ rs)

toDelegate
    :: F.Handler' v s a c
    -> F.Executor' c
    -> TMVar v
    -> ReifiedLens' v s -- reified because output doesn't have lens type variables
    -> a
    -> MaybeT IO ()
toDelegate (F.Handler' hdl) (F.Executor' exec) v l a = hoist atomically (hdl v l a) >>= exec

toCallbacks
    :: NFData a
    => D.DList (F.Trigger' a)
    -> (TMVar v -> ReifiedLens' v s -> a -> MaybeT IO ())
    -> TMVar v
    -> ReifiedLens' v s -- reified because output doesn't have lens type variables
    -> [(J.JSString, J.JSVal -> IO ())]
toCallbacks ts delegate v l = go <$> (D.toList ts)
  where
    go (F.Trigger' (evt, t)) = (evt, fmap (fromMaybe ()) . runMaybeT . R.handleEventM t (delegate v l))

-- | Create a Prototype from an Archetype.
-- This wraps the specifications and requirements in an additional layer of 'Many'.
-- Therefore, this is NOT the opposite of 'commission', that is:
--
-- @
-- redraft . commission /= id
-- @
redraft
    :: ( UniqueMember r reqs, UniqueMember s specs)
    => Archetype v r s
    -> F.Prototype v '[r] reqs '[s] specs '[] '[] acts '[] '[] cmds
redraft (Archetype (mkEnt, frmEnt, rnd)) = F.Prototype
    ( F.Builder (mkSpec, fromSpec)
    , F.divIfNeeded disp
    , mempty
    , mempty
    , mempty)
  where
    mkSpec v (Lens l) rs = do
        let r = fetch rs
        single <$> mkEnt v (Lens (l . F.specifications . item)) r
    fromSpec ss = let s = fetch ss in single <$> frmEnt s
    disp d = let s = d ^. (F.specifications . item) in rnd s

prismAction :: Prism' a' a -> Archetyper v r s a c -> Archetyper v r s a' c
prismAction l f hdl = f (lmap (review l) hdl)

isoCommand :: Iso' c' c -> Archetyper v r s a c -> Archetyper v r s a c'
isoCommand l f hdl exec = f (fmap (view l) hdl) (contramap (review l) exec)

isoSpecification :: Iso' s' s -> Archetype v r s -> Archetype v r s'
isoSpecification l (Archetype (mkEnt, frmEnt, disp)) =
        Archetype ( \v (Lens l') r -> review l <$> mkEnt v (Lens (l' . l)) r
                  , frmEnt . view l
                  , magnify l disp)

isoSpecification' :: Iso' s' s -> Archetyper v r s a c -> Archetyper v r s' a c
isoSpecification' l f hdl exec = isoSpecification l (f (F.handleUnder' (from l) hdl) exec)

isoRequirement :: Iso' r' r -> Archetype v r s -> Archetype v r' s
isoRequirement l (Archetype (mkEnt, frmEnt, disp)) =
        Archetype ( \v l' r -> mkEnt v l' (view l r)
                  , fmap (review l) . frmEnt
                  , disp)

isoRequirement' :: Iso' r' r -> Archetyper v r s a c -> Archetyper v r' s a c
isoRequirement' l f hdl exec = isoRequirement l (f hdl exec)
