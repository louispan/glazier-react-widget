{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Prototype where

import Data.Diverse
import Data.Kind
import Data.Semigroup
import qualified Glazier.React.Framework.Build as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Execute as F
import qualified Glazier.React.Framework.Firsts as F
import qualified Glazier.React.Framework.Gadgetry as F
import qualified Glazier.React.Framework.Trigger as F

newtype Prototype m output
                    (o :: [Type]) ols
                    (d :: [Type]) dtls
                    (p :: [Type]) plns
                    (t :: [Type]) trigs
                    -- actions from Trigger vs actions handled by Gadgetry
                    (a :: [Type]) (a' :: [Type]) acts
                    -- commands from Gadgetry vs commands handled by Execute
                    (c :: [Type]) (c' :: [Type]) cmds
                    (e :: [Type]) envs
    = Prototype ( F.Build o ols d dtls p plns acts
                , F.Display dtls plns
                , F.Trigger t trigs a acts
                , F.Gadgetry dtls plns a' acts c cmds
                , F.Execute m output acts c' cmds e envs )

-- | The action and command types are merged, not appended
andPrototype
    :: Applicative m
    => Prototype m output o1 ols d1 dtls p1 plns t1 trigs a1 a1' acts c1 c1' cmds e1 envs
    -> Prototype m output o2 ols d2 dtls p2 plns t2 trigs a2 a2' acts c2 c2' cmds e2 envs
    -> Prototype m output
                 (Append o1 o2) ols
                 (Append d1 d2) dtls
                 (Append p1 p2) plns
                 (AppendUnique t1 t2) trigs
                 (AppendUnique a1 a2) (AppendUnique a1' a2') acts
                 (AppendUnique c1 c2) (AppendUnique c1' c2') cmds
                 (AppendUnique e1 e2) envs
andPrototype (Prototype (b, d, t, g, e)) (Prototype (b', d', t', g', e')) =
    Prototype ( b `F.andBuild` b'
              , d <> d'
              , t `F.andTrigger` t'
              , g `F.andGadgetry` g'
              , e `F.andExecute` e')

orPrototype
    :: Applicative m
    => Prototype m output o1 ols d1 dtls p1 plns t1 trigs a1 a1' acts c1 c1' cmds e1 envs
    -> Prototype m output o2 ols d2 dtls p2 plns t2 trigs a2 a2' acts c2 c2' cmds e2 envs
    -> Prototype m output
                 (Append o1 o2) ols
                 (Append d1 d2) dtls
                 (Append p1 p2) plns
                 (AppendUnique t1 t2) trigs
                 (AppendUnique a1 a2) (AppendUnique a1' a2') acts
                 (AppendUnique c1 c2) (AppendUnique c1' c2') cmds
                 (AppendUnique e1 e2) envs
orPrototype (Prototype (b, d, t, g, e)) (Prototype (b', d', t', g', e')) =
         Prototype ( b `F.andBuild` b'
                   , d F.<<|>> d'
                   , t `F.orTrigger` t'
                   , g `F.orGadgetry` g'
                   , e `F.andExecute` e')

instance Applicative m =>
         Semigroup (Prototype m output o ols d dtls p plns t trigs a a' acts c c' cmds e envs) where
    (Prototype (b, d, t, g, e)) <> (Prototype (_, d', t', g', e')) =
        Prototype (b, d <> d', t <> t', g <> g', e <> e')

-- | identity for 'andPrototype' and 'orPrototype'
blank :: Applicative m => Prototype m output
                                    '[] ols
                                    '[] dtls
                                    '[] plns
                                    '[] trigs
                                    '[] '[] acts
                                    '[] '[] cmds
                                    '[] envs
blank = Prototype (F.idle, mempty, F.boring, F.noop, F.ignore)

statically
    :: Applicative m
    => F.Display dtls plns
    -> Prototype m output
                 '[] ols
                 '[] dtls
                 '[] plns
                 '[] trigs
                 '[] '[] acts
                 '[] '[] cmds
                 '[] envs
statically d = Prototype (F.idle, d, F.boring, F.noop, F.ignore)

dynamically
    :: Applicative m
    => F.Gadgetry dtls plns a acts c cmds
    -> Prototype m output
                 '[] ols
                 '[] dtls
                 '[] plns
                 '[] trigs
                 '[] a acts
                 c '[] cmds
                 '[] envs
dynamically g = Prototype ( F.idle
                          , mempty
                          , F.boring
                          , g
                          , F.ignore)

triggering
    :: Applicative m
    => F.Trigger t trigs a acts
    -> Prototype m output
                 '[] ols
                 '[] dtls
                 '[] pln
                 t trigs
                 a '[] acts
                 '[] '[] cmds
                 '[] envs
triggering t = Prototype (F.idle, mempty, t, F.noop, F.ignore)

building
    :: Applicative m
    => F.Build o ols d dtls p plns acts
    -> Prototype m output
                 o ols
                 d dtls
                 p plns
                 '[] trigs
                 '[] '[] acts
                 '[] '[] cmds
                 '[] envs
building b = Prototype (b, mempty, F.boring, F.noop, F.ignore)

executing
    :: F.Execute m output acts c cmds e envs
    -> Prototype m output
                 '[] ols
                 '[] dtls
                 '[] plns
                 '[] trigs
                 '[] '[] acts
                 '[] c cmds
                 e envs
executing e = Prototype (F.idle, mempty, F.boring, F.noop, e)
