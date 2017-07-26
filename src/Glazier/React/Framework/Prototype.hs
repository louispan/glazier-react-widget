{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Prototype
    ( Prototype(..)
    , dummy
    , statically
    , dynamically
    -- , attributes
    -- , componentize
    ) where

import Control.Applicative
import Control.Lens
import Data.Coerce
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Kind
import Data.Proxy
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React.Framework.Attach as F
import qualified Glazier.React.Framework.Widget as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Gizmo as F
import qualified JavaScript.Extras as JE

newtype Prototype (o :: [Type]) (d :: [Type]) (p :: [Type]) (a :: [Type]) (c :: [Type]) ols dtls plns v acts cmds = Prototype
    { runPrototype :: (Proxy a, Proxy c, F.Display dtls plns, F.Gizmo o d p ols dtls plns v acts cmds)
    }

-- | The action and command types are merged, not appended
instance (o3 ~ Append o1 o2, d3 ~ Append d1 d2, p3 ~ Append p1 p2, a3 ~ AppendUnique a1 a2, c3 ~ AppendUnique c1 c2) =>
         F.Attach (Prototype o1 d1 p1 a1 c1 ols dtls plns v acts cmds)
                (Prototype o2 d2 p2 a2 c2 ols dtls plns v acts cmds)
                (Prototype o3 d3 p3 a3 c3 ols dtls plns v acts cmds) where
     Prototype (_, _, disp, g) +<>+ Prototype (_, _, disp', g') =
         Prototype (Proxy, Proxy, disp <> disp', g F.+<>+ g')
     Prototype (_, _, disp, g) +<|>+ Prototype (_, _, disp', g') =
         Prototype (Proxy, Proxy, disp <> disp', g F.+<|>+ g')

instance F.AttachId (Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds) where
    aempty = dummy

-- | identity for 'F.Attach'
dummy :: Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds
dummy = Prototype (Proxy, Proxy, F.blank, F.noop)

statically :: F.Display dtls plns -> Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds
statically disp = Prototype (Proxy, Proxy, disp, F.aempty)

dynamically :: F.Gizmo '[] '[] '[] ols dtls plns v acts cmds -> Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds
dynamically gad = Prototype (Proxy, Proxy, mempty, gad)

-- -- | Wrap an 'Glazier.React.Component' (with its own render and dispose functions) around a 'Prototype'
-- -- replace original dtls plns with Entity.
-- componentize
--     :: forall dtls' plns' ols dtls plns v acts cmds.
--     ( UniqueMember F.ComponentAction acts
--     , UniqueMember F.ComponentCommand cmds
--     , UniqueMember (F.Entity dtls plns v) dtls')
--     => Prototype ols dtls plns acts cmds ols dtls plns v acts cmds
--     -> Prototype ols '[F.Entity' dtls plns] '[] acts cmds ols dtls' plns' v acts cmds
-- componentize (Prototype (pa, pc, dsp, F.Gizmo (mkDtl, toOl, mkPln, dev))) = Prototype
--     ( pa
--     , pc
--     , F.divWrapped F.componentWindow
--     , F.Gizmo (mkDtl', toOl', pure nil, dev'))
--   where
--     w' = F.renderDisplay dsp
--     mkDtl' o = do
--         d <- mkDtl o
--         single <$> F.mkEntity' d mkPln w'
--     toOl' d = toOl (d ^. item @(F.Entity dtls plns v) . F.details)
--     dev' = zoom (F.details . item @(F.Entity dtls plns v)) dev <|> componentGadget'
--     componentGadget' =
--         fmap pick <$>
--         magnify
--             (facet @F.ComponentAction)
--             (zoom (F.details . item @(F.Entity dtls plns v)) F.componentGadget)

-- -- | Add the ability to retrieved a list of properties from its corresponding detail/outline for the rendered element.
-- attributes
--     :: forall ols dtls plns v acts cmds.
--        (UniqueMember (M.Map J.JSString JE.JSVar) dtls, UniqueMember (M.Map J.JSString JE.JSVar) ols)
--     => Prototype '[M.Map J.JSString JE.JSVar] '[M.Map J.JSString JE.JSVar] '[] '[] '[] ols dtls plns v acts cmds
-- attributes = Prototype
--     ( Proxy
--     , Proxy
--     , F.Display
--           ( \s ->
--                 let m = s ^. F.details . item @(M.Map J.JSString JE.JSVar)
--                 in D.fromList . coerce . M.toList $ m
--           , mempty
--           , Nothing)
--     , F.Gizmo
--           ( \o -> pure (single $ o ^. item @(M.Map J.JSString JE.JSVar))
--           , \d -> single $ d ^. item @(M.Map J.JSString JE.JSVar)
--           , pure nil
--           , empty))

-- -- | Add a single property and its corresponding detail/outline to the rendered element.
-- withDynamicProperty
--     :: forall t ols dtls plns v acts cmds proxy.
--        (Show t, UniqueMember (t, JE.JSVar) dtls, UniqueMember (t, JE.JSVar) ols)
--     => proxy t
--     -> Prototype '[(t, JE.JSVar)] '[(t, JE.JSVar)] '[] '[] '[] ols dtls plns v acts cmds
-- withDynamicProperty _ = Prototype
--     ( Proxy
--     , Proxy
--     , F.Display
--           ( \s ->
--                 let (t, v) = s ^. F.details . item @(t, JE.JSVar)
--                 in D.singleton $ F.WindowProperty (JS.pack . lowerFirstLetter . show $ t, v)
--           , mempty
--           , Nothing)
--     , F.Gizmo
--           ( \o -> pure (single $ o ^. item @(t, JE.JSVar))
--           , \d -> single $ d ^. item @(t, JE.JSVar)
--           , pure nil
--           , empty))
--   where
--     lowerFirstLetter [] = []
--     lowerFirstLetter (x : xs) = toLower x : xs
