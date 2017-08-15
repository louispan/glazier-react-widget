{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Foldable
import Data.Proxy
import qualified Data.Map.Strict as M
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Gadgetry as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.TypeLevel as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC

-- | NB. o must contain [JE.Property], a must contain WidgetAction, c must contain WidgetCommand
newtype Archetype m o s a c e = Archetype ( o -> F (R.Reactor a) s
                                          , s -> STM o
                                          , G.WindowT s (R.ReactMlT STM) ()
                                          , PC.Output a -> s -> G.WindowT a STM c
                                          , e -> c -> MaybeT m ())

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to o, WidgetAction to a, WidgetCommand to c, and also add widgetGadget.
-- but the Prototype must already have an Execute for WidgetCommand
commission ::
    ( Monad m
    , UniqueMember [JE.Property] o'
    , UniqueMember F.WidgetAction a'
    , UniqueMember F.WidgetCommand c'
    , o' ~ ([JE.Property] ': o)
    , a' ~ (AppendUnique '[F.WidgetAction] a1)
    , c' ~ (AppendUnique '[F.WidgetCommand] c1) -- Despite GHC warning, this is not redundant constraint
    , F.SameMembers a1 a2
    , F.SameMembers c2 c'
    )
    => F.Prototype m
                   o o'
                   d d
                   p p
                   t t
                   a1 a2 a'
                   c1 c2 c'
                   e e
    -> Archetype m (Many o') (F.Entity d p) (Which a') (D.DList (Which c')) (Many e)
commission (F.Prototype ( F.Builder (mkDtl, fromDtl, mkPln)
                      , d
                      , F.Trigger (_, _, t)
                      , F.Gadgetry (_, _, g)
                      , F.Executor (_, _, e))) =
    Archetype ( mkEnt
              , fromEnt
              , F.inTVar F.widgetWindow
              , \out s -> F.withTVar s (g' <|> g out)
              , \env cmds -> traverse_ (e env) (D.toList cmds))
  where
    g' = magnify facet (fmap pick <$> F.widgetGadget)
    w' = F.inTVar (F.renderDisplay d)
    mkEnt o = do
        dtls <- mkDtl o
        let ps = o ^. item -- @[JE.Property]
        F.mkEntity ps dtls hls mkPln w'
    fromEnt ent = do
        ent' <- readTVar ent
        let ps = ent' ^. F.properties
        (./) <$> pure ps <*> fromDtl (ent' ^. F.details)
    hls = M.toList ((\(f, f') a -> f a >>= f' >>= pure . D.toList) <$> t)

-- | Create a Prototype from an Archetype.
-- NB. This is NOT the opposite of 'complete'.
--
-- @
-- redraft . complete /= id
-- @
redraft
    :: ( Monad m
       , UniqueMember o ols
       , UniqueMember s dtls
       , UniqueMember a acts
       , UniqueMember c cmds
       , UniqueMember e envs
       )
    => Archetype m o s a c e
    -> F.Prototype m '[o] ols '[s] dtls '[] plns '[] trigs '[] '[a] acts '[c] '[c] cmds '[e] envs
redraft (Archetype (mkEnt, fromEnt, disp, gad, e)) = F.Prototype
    ( F.Builder (mkDtl, fromDtl, mkPln)
    , F.divWrapped (magnify (F.details . item) disp)
    , F.boring
    , F.gadgetry (\out -> D.singleton <$> zoom (F.details . item) (gad' (contramap pick out)))
    , F.executor Proxy e')
  where
    gad' out = review G._GRMST' $ \a s -> (\c -> (c, s)) <$> view G._WRMT' (gad out s) a
    mkDtl o = let o' = fetch o in R.hoistWithAction pick (single <$> mkEnt o')
    fromDtl d = let d' = fetch d in single <$> fromEnt d'
    mkPln = pure nil
    e' env = e (fetch env)

mapEnvironment :: (e' -> e) -> Archetype m o s a c e -> Archetype m o s a c e'
mapEnvironment f (Archetype (mkEnt, fromEnt, disp, gad, e)) =
        Archetype (mkEnt, fromEnt, disp, gad, \env cmd -> e (f env) cmd)

dispatchAction
    :: Monoid c
    => Prism' a' a
    -> Archetype m o s a c e
    -> Archetype m o s a' c e
dispatchAction l (Archetype (mkEnt, fromEnt, disp, gad, e)) =
        Archetype ( R.hoistWithAction (review l) . mkEnt
                  , fromEnt
                  , disp
                  , \out s -> magnify l (gad (contramap (review l) out) s)
                  , e)

translateCommand :: Iso' c' c -> Archetype m o s a c e -> Archetype m o s a c' e
translateCommand l (Archetype (mkEnt, fromEnt, disp, gad, e)) =
        Archetype ( mkEnt
                  , fromEnt
                  , disp
                  , \out s -> review l <$> gad out s
                  , \env cmd -> e env (view l cmd))

translateState :: Iso' s' s -> Archetype m o s a c e -> Archetype m o s' a c e
translateState l (Archetype (mkEnt, fromEnt, disp, gad, e)) =
        Archetype ( \o -> fmap (review l) (mkEnt o)
                  , fromEnt . view l
                  , magnify l disp
                  , \out s -> gad out (view l s)
                  , e)

translateOutline :: Iso' o' o -> Archetype m o s a c e -> Archetype m o' s a c e
translateOutline l (Archetype (mkEnt, fromEnt, disp, gad, e)) =
        Archetype ( mkEnt . view l
                  , \s -> review l <$> fromEnt s
                  , disp
                  , gad
                  , e)
