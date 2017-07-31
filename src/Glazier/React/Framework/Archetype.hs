{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype where

import Control.Applicative
import Control.Lens
import Control.Monad.Free.Church
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Build as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Execute as F
import qualified Glazier.React.Framework.Gadgetry as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.TypeLevel as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

-- | NB. o must contain [JE.Property], a must contain WidgetAction, c must contain WidgetCommand
-- The window function is always F.widgetWindow, so it doesn't need to be stored
newtype Archetype m o s a c e = Archetype ( o -> F (R.Maker a) s
                                          , s -> o
                                          , G.Gadget a s c
                                          , e -> c -> m ())

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to o, WidgetAction to a, WidgetCommand to c, and also add widgetGadget.
-- but the Prototype must already have an Execute for WidgetCommand
commission ::
    ( Applicative m
    , UniqueMember [JE.Property] o'
    , UniqueMember F.WidgetAction a'
    , UniqueMember F.WidgetCommand c'
    , o' ~ ([JE.Property] ': o)
    , a' ~ (AppendUnique '[F.WidgetAction] a1)
    , c' ~ (AppendUnique '[F.WidgetCommand] c1) -- Despite GHC warning, this is not redundant constraint
    , F.SameMembers a1 a2
    , F.SameMembers c2 c'
    )
    => F.Prototype m o o'
                     d d
                     p p
                     t t
                     a1 a2 a'
                     c1 c2 c'
                     e e
    -> Archetype m (Many o') (F.Entity d p) (Which a') (D.DList (Which c')) (Many e)
commission (F.Prototype ( F.Build (mkDtl, fromDtl, mkPln)
                        , d
                        , F.Trigger (_, _, t)
                        , F.Gadgetry (_, _, g)
                        , F.Execute (_, _, e))) =
    Archetype (mkEnt, fromEnt, g' <|> g, \env cmds -> traverse_ (e env) (D.toList cmds))
  where
    g' = magnify facet (fmap pick <$> F.widgetGadget)
    w' = F.renderDisplay d
    mkEnt o = do
        dtls <- mkDtl o
        let ps = o ^. item -- @[JE.Property]
        F.mkEntity ps dtls hls mkPln w'
    fromEnt ent =
        let ps = ent ^. F.properties
        in ps ./ fromDtl (ent ^. F.details)
    hls = M.toList ((\(f, f') a -> f a >>= f' >>= pure . D.toList) <$> t)


mapEnvironment :: (e' -> e) -> Archetype m o s a c e -> Archetype m o s a c e'
mapEnvironment f (Archetype (mkEnt, fromEnt, gad, e)) =
        Archetype (mkEnt, fromEnt, gad, \env cmd -> e (f env) cmd)

dispatchAction :: Monoid c => Prism' a' a -> Archetype m o s a c e -> Archetype m o s a' c e
dispatchAction l (Archetype (mkEnt, fromEnt, gad, e)) =
        Archetype ( R.hoistWithAction (review l) . mkEnt
                  , fromEnt
                  , magnify l gad
                  , e)

translateCommand :: Iso' c' c -> Archetype m o s a c e -> Archetype m o s a c' e
translateCommand l (Archetype (mkEnt, fromEnt, gad, e)) =
        Archetype (mkEnt, fromEnt, review l <$> gad, \env cmd -> e env (view l cmd))

translateState :: Iso' s' s -> Archetype m o s a c e -> Archetype m o s' a c e
translateState l (Archetype (mkEnt, fromEnt, gad, e)) =
        Archetype ( fmap (review l) . mkEnt
                  , fromEnt . view l
                  , zoom l gad
                  , e)

translateOutline :: Iso' o' o -> Archetype m o s a c e -> Archetype m o' s a c e
translateOutline l (Archetype (mkEnt, fromEnt, gad, _)) =
        Archetype ( mkEnt . view l
                  , review l . fromEnt
                  , gad
                  , undefined)
