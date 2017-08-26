{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Semigroup
import Control.Monad.Morph
import Data.Maybe
import Data.Proxy
import qualified Data.Map.Strict as M
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
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

type Archetyper v r s a = F.Handler v s a a -> Archetype v r s

mkBasicEntity :: (TMVar s -> ReifiedLens' s s -> r -> F R.Reactor s) -> r -> F R.Reactor (TMVar s)
mkBasicEntity mkEnt r = do
    v <- R.doSTM newEmptyTMVar
    s <- mkEnt v (Lens id) r
    R.doSTM $ putTMVar v s
    pure v

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
commission
    :: (NFData (Which a), UniqueMember [JE.Property] r', r' ~ ([JE.Property] ': r))
    => F.Prototype v r r s s a a
    -> F.Handler v (F.Design s) a a
    -> Archetype v (Many r') (F.Design s)
commission (F.Prototype (F.Builder (mkSpec, fromSpec), disp, ts)) hdl =
    Archetype (mkEnt, frmEnt, F.componentWindow)
  where
    cbs = toCallbacks ts hdl
    mkEnt v l@(Lens l') rs = do
        let (ps, xs) = viewf rs
        ss <- mkSpec v l xs
        d <- F.mkDesign ps ss w cbs v l'
        pure d
    w = F.renderDisplay (F.widgetDisplay <> disp)
    frmEnt d = do
        let ps = d ^. F.properties
            ss = d ^. F.specifications
        rs <- fromSpec ss
        pure (ps ./ rs)

toCallbacks
    :: NFData (Which a)
    => F.Trigger a a
    -> F.Handler v s a a
    -> TMVar v
    -> Lens' v s
    -> [(J.JSString, J.JSVal -> IO ())]
toCallbacks (F.Trigger (_, ts)) (F.Handler (_, hdl)) v l = cbs'
  where
    hdl' = hdl v (Lens l)
    hdl'' a = hoist atomically (hdl' a)
    go (evt, t) = (evt, fmap (fromMaybe ()) . runMaybeT . R.handleEventM t hdl'')
    cbs = go <$> D.toList ts
    cbs' = M.toList (M.fromListWith combineCbs cbs)
    combineCbs = liftA2 (>>)

-- | Create a Prototype from an Archetype.
-- This wraps the specifications and requirements in an additional layer of 'Many'.
-- Therefore, this is NOT the opposite of 'comission', that is:
--
-- @
-- redraft . commission /= id
-- @
redraft
    :: ( UniqueMember r reqs, UniqueMember s specs)
    => Archetype v r s
    -> F.Prototype v '[r] reqs '[s] specs '[] acts
redraft (Archetype (mkEnt, frmEnt, rnd)) = F.Prototype
    ( F.Builder (mkSpec, fromSpec)
    , F.divIfNeeded disp
    , F.boring)
  where
    mkSpec v (Lens l) rs = do
        let r = fetch rs
        single <$> mkEnt v (Lens (l . F.specifications . item)) r
    fromSpec ss = let s = fetch ss in single <$> frmEnt s
    disp d = let s = d ^. (F.specifications . item) in rnd s

dispatch :: Prism' (Which a') (Which a) -> Archetyper v r s a -> Archetyper v r s a'
dispatch l g (F.Handler (_, f)) = g (F.Handler (Proxy, \v l' a -> f v l' (review l a)))

implantS :: Iso' s' s -> Archetype v r s -> Archetype v r s'
implantS l (Archetype (mkEnt, frmEnt, disp)) =
        Archetype ( \v (Lens l') r -> review l <$> mkEnt v (Lens (l' . l)) r
                  , frmEnt . view l
                  , magnify l disp)

implantS' :: Iso' s' s -> Archetyper v r s a -> Archetyper v r s' a
implantS' l f hdl = implantS l (f (F.handleUnder (from l) hdl))

implantR :: Iso' r' r -> Archetype v r s -> Archetype v r' s
implantR l (Archetype (mkEnt, frmEnt, disp)) =
        Archetype ( \v l' r -> mkEnt v l' (view l r)
                  , fmap (review l) . frmEnt
                  , disp)

implantR' :: Iso' r' r -> Archetyper v r s a -> Archetyper v r' s a
implantR' l f = implantR l . f
