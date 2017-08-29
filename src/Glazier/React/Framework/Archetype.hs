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
 , implement
 , implement'
 , implant
 -- , tweakAction
 -- , tweakCommand
 -- , tweakSpecification
 -- , tweakSpecification'
 -- , tweakRequirement
 -- , tweakRequirement'
 ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
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

newtype Archetype v r s ctx = Archetype ( s -> R.ReactMlT STM ()
                                        , s -> STM r
                                        , ctx -> TMVar v -> ReifiedLens' v s -> r -> F R.Reactor s)

-- | Used where an externally given delegate is required to 'commission'' an 'Archetype'
type Archetyper v r s a c = Archetype v r s (F.Handler' v s a c, F.Executor' c)

mkBasicEntity :: (TMVar s -> ReifiedLens' s s -> r -> F R.Reactor s) -> r -> F R.Reactor (TMVar s)
mkBasicEntity mkEnt r = do
    v <- R.doSTM newEmptyTMVar
    s <- mkEnt v (Lens id) r
    R.doSTM $ putTMVar v s
    pure v

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
implement
    :: ( NFData (Which a)
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Prototype v r r s s a a a c c c
    -> Archetype v (Many r') (F.Design s) ()
implement (F.Prototype (bldr, disp, ts, hdl, exec)) = Archetype
  ( F.componentWindow
  , fromEntity bldr
  , const (mkEntity bldr disp ts delegate)
  )
  where
    delegate = toDelegate (F.getHandler hdl) (F.getExecutor exec) -- internal handling

-- | A variation of 'commission' where the Prototype has incomplete handlers
-- and so required external handlers to complete the Archetype.
implement'
    :: ( Reinterpret' h a
       , NFData (Which a)
       , UniqueMember [JE.Property] r'
       , r' ~ ([JE.Property] ': r))
    => F.Prototype v r r s s a (Complement a h) a c c c
    -> Archetyper v (Many r') (F.Design s) h c'
implement' (F.Prototype (bldr, disp, ts, hdl, exec)) = Archetype
  ( F.componentWindow
  , fromEntity bldr
  , mkEnt)
  where
    internalDelegate = toDelegate (F.getHandler hdl) (F.getExecutor exec) -- internal handling
    mkEnt (hdl', exec') = mkEntity bldr disp ts delegate
      where
        externalDelegate v l a = case reinterpret' a of
            Nothing -> empty
            Just a' -> toDelegate hdl' exec' v l a'
        delegate v l a = internalDelegate v l a <|> externalDelegate v l a

mkEntity
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
    -> TMVar v
    -> ReifiedLens' v (F.Design s)
    -> (Many r')
    -> F R.Reactor (F.Design s)
mkEntity (F.Builder (_, mkSpec)) disp ts delegate v l@(Lens l') rs = do
        let (ps, xs) = viewf rs
        ss <- mkSpec v l xs
        d <- F.mkDesign ps ss w cbs' v l'
        pure d
  where
    cbs = toCallbacks (F.getTriggers ts) delegate
    cbs' v' l'' = M.toList . M.fromListWith (liftA2 (>>)) $ cbs v' l''
    w = F.renderDisplay (F.widgetDisplay <> disp)

fromEntity :: (UniqueMember [JE.Property] r', r' ~ ([JE.Property] ': r))
    => F.Builder v r r s s -> F.Design s -> STM (Many r')
fromEntity (F.Builder (fromSpec, _)) d = do
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

-- | Create a Prototype from an Archetype.
-- This wraps the specifications and requirements in an additional layer of 'Many'.
-- Therefore, this is NOT the opposite of 'implement', that is:
--
-- @
-- implant . implement /= id
-- @
implant
    :: ( UniqueMember r reqs, UniqueMember s specs)
    => Archetype v r s ()
    -> F.Prototype v '[r] reqs '[s] specs '[] '[] acts '[] '[] cmds
implant (Archetype (rnd, frmEnt, mkEnt)) = F.Prototype
    ( F.Builder (fromSpec, mkSpec)
    , F.divIfNeeded disp
    , mempty
    , mempty
    , mempty)
  where
    mkSpec v (Lens l) rs = do
        let r = fetch rs
        single <$> mkEnt () v (Lens (l . F.specifications . item)) r
    fromSpec ss = let s = fetch ss in single <$> frmEnt s
    disp d = let s = d ^. (F.specifications . item) in rnd s

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
