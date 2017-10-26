{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Glazier.React.Framework.Widget where

import Data.Diverse
import Data.Kind
-- import Data.Proxy
import Data.Semigroup (Semigroup(..))
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F

-- (t + a) is the set of actions that need to be handled.
-- h is the set of action handlers.
-- h can be smaller than (t + a) if we are given external handlers for (t + a - h)
-- t can be smaller than h but (t - h) must be included inside p, otherwise a compile error (for best practice)
-- The external event interface of Prototype is that it exposes a Handler for p
-- p must contain only things inside h. It cannot contain things that are handled by external handlers.

newtype Widget m p s v a b r =
    Widget ( F.Builder m p s p s
           , F.Handler m v s a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.Activator m b v s
           , F.Display m s r
           ) deriving Functor

-- instance Monad m => Applicative (Widget m p s v a b)
--     pure a = Widget (F.Bu

-- -- | identity for 'andPrototype'
-- dummy :: Monad m => Prototype m '[] reqs '[] specs '[] hs '[] ts '[] as '[] cs '[]
-- dummy = Prototype (mempty, F.idle, F.ignore, F.boring, F.inert)

-- -- | The action and command types are merged, not appended
-- andPrototype
--     :: Monad m
--     => Prototype m r1 reqs s1 specs h1 hs t1 ts a1 as c1 cs cmds
--     -> Prototype m r2 reqs s2 specs h2 hs t2 ts a2 as c2 cs cmds
--     -> Prototype m (Append r1 r2) reqs
--                    (Append s1 s2) specs
--                    (Append h1 h2) hs
--                    (AppendUnique t1 t2) ts
--                    (AppendUnique a1 a2) as
--                    (AppendUnique c1 c2) cs cmds
-- andPrototype (Prototype (d, b, h, t, a)) (Prototype (d', b', h', t', a')) =
--     Prototype
--         ( d <> d'
--         , b `F.andBuilder` b'
--         , h `F.orHandler` h'
--         , t `F.andTriggers` t'
--         , a `F.andActivator` a'
--         )

-- displaying
--     :: Monad m
--     => F.Display m specs
--     -> Prototype m '[] reqs '[] specs '[] hs '[] ts '[] as '[] cs cmds
-- displaying d = Prototype (d, F.idle, F.ignore, F.boring, F.inert)

-- building
--     :: Monad m
--     => F.Builder m r reqs s specs
--     -> Prototype m r reqs s specs '[] hs '[] ts '[] as '[] cs cmds
-- building b = Prototype (mempty, b, F.ignore, F.boring, F.inert)

-- activating
--     :: Monad m
--     => F.Activator m (F.Design specs) a as cmds
--     -> Prototype m '[] reqs '[] specs '[] hs '[] ts a as '[] cs cmds
-- activating a = Prototype (mempty, F.idle, F.ignore, F.boring, a)

-- triggering
--     :: Monad m
--     => F.Triggers t ts
--     -> Prototype m '[] reqs '[] specs '[] hs t ts '[] as '[] cs cmds
-- triggering t = Prototype (mempty, F.idle, F.ignore, t, F.inert)

-- handling
--     :: Monad m
--     => F.Handler m (F.Design specs) h hs c cs
--     -> Prototype m '[] reqs '[] specs h hs '[] ts '[] as c cs cmds
-- handling h = Prototype (mempty, F.idle, h, F.boring, F.inert)
