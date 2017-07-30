{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype where

import Control.Lens
import Control.Monad.Free.Church
import Data.Diverse.Lens
import qualified Data.DList as D
import qualified Data.Map.Strict as M
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Create as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Gadgetry as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

-- | NB. o must contain [JE.Property], a must contain WidgetAction, c must contain WidgetCommand
newtype Archetype o s a c = Archetype (G.Gadget a s c, o -> F (R.Maker a) s, s -> o)

-- | Finalize the design of a 'Prototype'
-- and convert the make functions into making an Entity'.
-- NB. the window function is just F.widgetWindow
-- This also adds [JE.Property] to o, WidgetAction to a, WidgetCommand to c
commission :: forall o d p t a c o' a' c'.
    ( UniqueMember [JE.Property] o'
    , UniqueMember F.WidgetAction a'
    , o' ~ ([JE.Property] ': o)
    , a' ~ (F.WidgetAction ': a)
    , c' ~ (F.WidgetCommand ': c) -- redundant constraint
    )
    => F.Prototype o o' d d p p t t a a' c c'
    -> Archetype (Many o') (F.Entity d p) (Which a') (D.DList (Which c'))
commission (F.Prototype (d, F.Gadgetry (_, _, g), F.Trigger (_, _, t), F.Create (mkDtl, fromDtl, mkPln))) =
    Archetype (g, mkEnt, fromEnt)
  where
    w' = F.renderDisplay d
    mkEnt o = do
        dtls <- mkDtl o
        let ps = o ^. item @[JE.Property]
        F.mkEntity ps dtls hls mkPln w'
    fromEnt e =
        let ps = e ^. F.properties
        in ps ./ fromDtl (e ^. F.details)
    hls = M.toList ((\(a, b) -> fmap (D.toList . b) <$> a) <$> t)

instance Functor (Archetype o s a) where
    fmap f (Archetype (gad, mkEnt, fromEnt)) = Archetype (f <$> gad, mkEnt, fromEnt)

dispatch :: Monoid c => Prism' a' a -> Archetype o s a c -> Archetype o s a' c
dispatch l (Archetype (gad, mkEnt, fromEnt)) =
        Archetype ( magnify l gad
                  , R.hoistWithAction (review l) . mkEnt
                  , fromEnt)

embed :: Iso' s' s -> Archetype o s a c -> Archetype o s' a c
embed l (Archetype (gad, mkEnt, fromEnt)) =
        Archetype ( zoom l gad
                  , fmap (review l) . mkEnt
                  , fromEnt . view l)

onto :: Iso' o' o -> Archetype o s a c -> Archetype o' s a c
onto l (Archetype (gad, mkEnt, fromEnt)) =
        Archetype ( gad
                  , mkEnt . view l
                  , review l . fromEnt)
