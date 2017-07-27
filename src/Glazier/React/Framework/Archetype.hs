{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype where

import Control.Lens
import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Gizmo as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

type MkEntity' dtls plns ols acts = Many ols -> F (R.Maker (Which acts)) (F.Entity' dtls plns)
type FromEntity' ols dtls plns = F.Entity' dtls plns -> Many ols

-- | NB. o must contain [JE.Property], a must contain WidgetAction, c must contain WidgetCommand
newtype Archetype (o :: [Type]) (d :: [Type]) (p :: [Type]) (a :: [Type]) (c :: [Type]) ols v acts cmds = Archetype
    { getArchetype :: (Proxy a, Proxy c, MkEntity' d p ols acts, FromEntity' o d p, F.Gadgetry d p v acts cmds)
    }

-- | Finalize the design of a 'Prototype'
-- and convert the make functions into making an Entity'.
-- NB. the window function is just F.widgetWindow
-- This also adds [JE.Property] to o, WidgetAction to a, WidgetCommand to c
commission :: forall o d p a c ols v acts cmds.
    ( UniqueMember F.WidgetAction acts
    , UniqueMember [JE.Property] ols
    )
    => F.Prototype o d p a c ols d p v acts cmds
    -> Archetype ([JE.Property] ': o) d p (F.WidgetAction ': a) (F.WidgetCommand ': c) ols v acts cmds
commission (F.Prototype (_, d, F.Trigger (_, t), F.Gizmo (mkDtl, fromDtl, mkPln, g))) = Archetype
    ( Proxy
    , Proxy
    , mkEnt
    , fromEnt
    , g)
  where
    w' = F.renderDisplay d
    mkEnt o = do
        dtls <- mkDtl o
        let ps = o ^. item @[JE.Property]
        F.mkEntity' ps dtls (M.toList t) mkPln w'
    fromEnt e = let ps = e ^. F.properties
                in ps ./ fromDtl (e ^. F.details)
--     dev' = zoom (F.details . item @(F.Entity dtls plns v)) dev <|> widgetGadget'
--     g' =
--         fmap pick <$>
--         magnify
--             (facet @F.WidgetAction)
--             (zoom (F.details . item @(F.Entity dtls plns v)) F.widgetGadget)
