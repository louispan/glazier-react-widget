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
    , triggering
    -- , attributes
    -- , componentize
    ) where

import Data.Diverse
import Data.Kind
import Data.Proxy
import Data.Semigroup
import qualified Glazier.React.Framework.Attach as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Gizmo as F
import qualified Glazier.React.Framework.Trigger as F

newtype Prototype (o :: [Type]) (d :: [Type]) (p :: [Type]) (a :: [Type]) (c :: [Type]) ols dtls plns v acts cmds = Prototype
    { runPrototype :: (Proxy c, F.Display dtls plns, F.Trigger a acts, F.Gizmo o d p ols dtls plns v acts cmds)
    }

-- | The action and command types are merged, not appended
instance (o3 ~ Append o1 o2, d3 ~ Append d1 d2, p3 ~ Append p1 p2, a3 ~ AppendUnique a1 a2, c3 ~ AppendUnique c1 c2) =>
         F.Attach (Prototype o1 d1 p1 a1 c1 ols dtls plns v acts cmds)
                (Prototype o2 d2 p2 a2 c2 ols dtls plns v acts cmds)
                (Prototype o3 d3 p3 a3 c3 ols dtls plns v acts cmds) where
     Prototype (_, d, t, g) +<>+ Prototype (_, d', t', g') =
         Prototype (Proxy, d <> d', t F.+<>+ t', g F.+<>+ g')
     Prototype (_, d, t, g) +<|>+ Prototype (_, d', t', g') =
         Prototype (Proxy, d <> d', t F.+<|>+ t', g F.+<|>+ g')

instance F.AttachId (Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds) where
    aempty = dummy

-- | identity for 'F.Attach'
dummy :: Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds
dummy = Prototype (Proxy, F.blank, F.ignore, F.noop)

statically :: F.Display dtls plns -> Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds
statically d = Prototype (Proxy, d, F.ignore, F.noop)

dynamically :: F.Gizmo '[] '[] '[] ols dtls plns v acts cmds -> Prototype '[] '[] '[] '[] '[] ols dtls plns v acts cmds
dynamically g = Prototype (Proxy, mempty, F.ignore, g)

triggering :: F.Trigger a acts -> Prototype '[] '[] '[] a '[] ols dtls plns v acts cmds
triggering t = Prototype (Proxy, mempty, t, F.noop)

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
