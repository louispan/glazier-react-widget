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
-- import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Semigroup
import Control.Monad.Morph
import Data.Maybe
-- import Data.Foldable
-- import Data.Proxy
import qualified Data.Map.Strict as M
-- import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
-- import qualified Glazier.React.Framework.Executor as F
-- import qualified Glazier.React.Framework.Gadgetry as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
-- import qualified Glazier.React.Framework.TypeLevel as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE
-- import qualified Pipes.Concurrent as PC
import Control.DeepSeq
import qualified GHCJS.Types as J

-- | NB. a must contain [JE.Property]
newtype Archetype v r s = Archetype ( TMVar v -> ReifiedLens' v s -> r -> F R.Reactor s
                                  , s -> STM r
                                  , s -> R.ReactMlT STM ())

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


-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
commission
    :: (NFData (Which a), UniqueMember [JE.Property] r', r' ~ ([JE.Property] ': r))
    => F.Prototype r r s s a a
    -> F.Handler v (F.Design s) a a
    -- -> F.Handler v (Many s) a a
    -> Archetype v (Many r') (F.Design s)
commission (F.Prototype (F.Builder (mkSpec, fromSpec), disp, ts)) hdl =
    Archetype (mkEntity, frmEntity, F.componentWindow)
  where
    -- cbs = toCallbacks ts (F.magnifyHandler F.specifications hdl)
    cbs = toCallbacks ts hdl
    mkEntity v (Lens l) rs = do
        let (ps, xs) = viewf rs
        ss <- mkSpec xs
        d <- F.mkDesign ps ss w cbs v l
        pure d
    w = F.renderDisplay (F.widgetDisplay <> disp)
    frmEntity d = do
        let ps = d ^. F.properties
            ss = d ^. F.specifications
        rs <- fromSpec ss
        pure (ps ./ rs)

-- type Archetyper = 

-- | Create a Prototype from an Archetype.
-- NB. This is NOT the opposite of 'comission', that is:
--
-- @
-- redraft . complete /= id
-- @
redraft
    :: ( UniqueMember r reqs
       , UniqueMember s specs
       )
    => Archetype s r s
    -> F.Prototype '[r] reqs '[s] specs '[] acts
redraft (Archetype (mkEntity, frmEntity, rnd)) = F.Prototype
    ( F.Builder (mkSpec, fromSpec)
    , F.divIfNeeded disp
    , F.boring)
  where
    mkSpec rs = do
        let r = fetch rs
        v <- R.doSTM newEmptyTMVar
        single <$> mkEntity v (Lens id) r
    fromSpec ss = let s = fetch ss in single <$> frmEntity s
    disp d = let s = d ^. (F.specifications . item) in rnd s

-- linkSpecification :: Iso' s' s -> Archetype r s -> Archetype r s'
-- linkSpecification l (Archetype (mkEntity, frmEntity, disp)) =
--         Archetype ( fmap (review l) . mkEntity
--                   , frmEntity . view l
--                   , magnify l disp)

-- linkRequirement :: Iso' r' r -> Archetype r s -> Archetype r' s
-- linkRequirement l (Archetype (mkEntity, frmEntity, disp)) =
--         Archetype ( mkEntity . view l
--                   , fmap (review l) . frmEntity
--                   , disp)





-- type Prototyper r s a h = F.Handler s h a -> F.Prototype r r s s a a a
-- type Archetyper r s s' a a' = (s -> a -> MaybeT STM (), Iso' s (TMVar (F.Design s')), Prism' a (Which a')) -> Archetype r s

-- -- | Given an incomplete prototype without all the handlers, return a fuction that will
-- -- return the completed prototype, when given handlers required to complete the prototype.
-- prototyping
--     :: (t ~ Append h1 h2)
--     => F.Prototype r reqs s specs t acts
--     -> F.Handler h2 acts
--     -> F.Prototype r reqs s specs t acts
-- prototyping p h = p `F.andPrototype` F.handling h

-- wack :: F.Prototype '[Int] reqs '[Int] specs '[Int, Bool] '[Int] acts
-- wack = undefined

-- wock = prototyping wack
-- type ArchetypeHandler = TMVar (F.Design specs) -> Which acts -> MaybeT STM ()
-- type ArchetypeHandler = TMVar s -> a -> MaybeT STM ()

-- type Archetyper r s ctx = (Which ctx -> MaybeT STM ()) -> Archetype r s

-- -- | 'commission' using an incomplete prototype.
-- commissioning'
--     :: ( NFData (Which a)
--        , UniqueMember [JE.Property] r'
--        , r' ~ ([JE.Property] ': r)
--        , a ~ Append h (Complement a h)
--        , Reinterpret' (Complement a h) a
--        )
--     => PC.Output (R.Disposable ())
--     -> F.Prototype r r s s a h a
--     -> (Which (Complement a h) -> MaybeT STM ()) -- F.Handler s (Complement a h) a
--     -> Archetype (Many r') (TMVar (F.Design s))
-- commissioning' dc p h = commission dc (prototyping p h')
--   where
--     h' = F.Handler (Proxy, \a -> case reinterpret' a of
--                            Nothing -> empty
--                            Just a' -> h a')


-- -- | 'commission' using an incomplete prototype.
-- commissioning
--     :: ( NFData (Which a)
--        , UniqueMember [JE.Property] r'
--        , r' ~ ([JE.Property] ': r)
--        , a ~ Append h (Complement a h)
--        )
--     => PC.Output (R.Disposable ())
--     -> F.Prototype r r s s a h a
--     -> F.Handler (Complement a h) a
--     -> Archetype (Many r') (TMVar (F.Design s))
-- commissioning dc p h = commission dc (prototyping p h)
