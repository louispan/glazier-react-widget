{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Gadgetry where

import Control.Applicative
import Control.Lens
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import Data.Semigroup
import qualified Data.DList as D
import qualified Glazier as G
import qualified Glazier.React.Framework.Firsts as F
import qualified Glazier.React.Framework.Widget as F

newtype Gadgetry dtls plns (a :: [Type]) acts (c :: [Type]) cmds =
    Gadgetry (Proxy a, Proxy c, G.Gadget (Which acts) (F.Entity dtls plns) (D.DList (Which cmds)))

andGadgetry
    :: Gadgetry dtls plns a1 acts c1 cmds
    -> Gadgetry dtls plns a2 acts c2 cmds
    -> Gadgetry dtls plns (Append a1 a2) acts (Append c1 c2) cmds
andGadgetry (Gadgetry (_, _, g)) (Gadgetry (_, _, g')) =
    Gadgetry (Proxy, Proxy, g <> g')

orGadgetry
    :: Gadgetry dtls plns a1 acts c1 cmds
    -> Gadgetry dtls plns a2 acts c2 cmds
    -> Gadgetry dtls plns (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
orGadgetry (Gadgetry (_, _, g)) (Gadgetry (_, _, g')) =
    Gadgetry (Proxy, Proxy, g <|> g')

instance F.Firsts (Gadgetry dtls plns a acts c cmds) where
    Gadgetry (_, _, g) <<|>> Gadgetry (_, _, g') = Gadgetry (Proxy, Proxy, g <|> g')

-- | Identify for 'orGadgetry' or 'andGadgetry'
noop :: Gadgetry dtls plns '[] acts '[] cmds
noop = Gadgetry (Proxy, Proxy, empty)

gadgetry
    :: forall a c dtls plns acts cmds.
       (UniqueMember a acts, UniqueMember c cmds)
    => G.Gadget a (F.Entity dtls plns) (D.DList c)
    -> Gadgetry dtls plns '[a] acts '[c] cmds
gadgetry g = Gadgetry (Proxy, Proxy, magnify (facet @a) (fmap (pick @_ @c) <$> g))
