{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Widget where

import Data.Diverse
import Data.IORef
import Data.Semigroup (Semigroup(..))
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Widget m v p s p' s' a b c =
    Widget ( F.Builder m p s p' s'
           , F.Handler m v s a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.Activator m v s c
           , F.Display m s ()
           )

instance R.MonadReactor m =>
         F.IORefModel (Widget m s p s p' s' a b c) (Widget m v p (IORef s) p' (IORef s') a b c) where
    ioRefModel (Widget (bld, hdl, act, disp)) = Widget
        ( F.ioRefModel bld
        , F.ioRefModel hdl
        , F.ioRefModel act
        , F.ioRefModel disp
        )

------------------------------------------

newtype PWidget m v p s psabc = PWidget {
    runPWidget :: Widget m v p s (P.At0 psabc) (P.At1 psabc) (P.At2 psabc) (P.At3 psabc) (P.At4 psabc)
    }

type instance P.PNullary (PWidget m v p s) (p', s', a, b, c) = Widget m v p s p' s' a b c

instance Monad m => P.PMEmpty (PWidget m p s v) (Many '[], Many '[], Which '[], Which '[], Which '[]) where
    pmempty = Widget
        ( P.pmempty
        , P.pmempty
        , P.pmempty
        , mempty)

instance ( Monad m
         , p3 ~ Append p1 p2
         , s3 ~ Append s1 s2
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         , Reinterpret' c3 c1
         , Reinterpret' c3 c2
         , c3 ~ AppendUnique c1 c2
         ) => P.PSemigroup (PWidget m v p s)
             (Many p1, Many s1, Which a1, Which b1, Which c1)
             (Many p2, Many s2, Which a2, Which b2, Which c2)
             (Many p3, Many s3, Which a3, Which b3, Which c3) where
    (Widget (bld1, hdl1, act1, disp1)) `pmappend` (Widget (bld2, hdl2, act2, disp2)) =
        Widget
        ( bld1 `P.pmappend` bld2
        , hdl1 `P.pmappend` hdl2
        , act1 `P.pmappend` act2
        , disp1 <> disp2
        )

------------------------------------------

building
    :: Monad m
    => F.Builder m p s p' s'
    -> Widget m v p s p' s' (Which '[]) (Which '[]) (Which '[])
building bld = Widget
        ( bld
        , P.pmempty
        , P.pmempty
        , mempty)

handling
    :: Monad m
    => F.Handler m v s a b
    -> Widget m v p s (Many '[]) (Many '[]) a b (Which '[])
handling hdl = Widget
        ( P.pmempty
        , hdl
        , P.pmempty
        , mempty)

activating
    :: Monad m
    => F.Activator m v s c
    -> Widget m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) c
activating act = Widget
        ( P.pmempty
        , P.pmempty
        , act
        , mempty)

displaying
    :: Applicative m
    => F.Display m s ()
    -> Widget m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) (Which '[])
displaying d = Widget
        ( P.pmempty
        , P.pmempty
        , P.pmempty
        , d)

------------------------------------------

mapBuilder
    :: (F.Builder m p1 s p1' s1' -> F.Builder m p2 s p2' s2')
    -> Widget m v p1 s p1' s1' a b c
    -> Widget m v p2 s p2' s2' a b c
mapBuilder f (Widget (bld, hdl, act, disp)) = Widget
                   ( f bld
                   , hdl
                   , act
                   , disp)

mapHandler
    :: (F.Handler m v s a1 b1 -> F.Handler m v s a2 b2)
    -> Widget m v p s p' s' a1 b1 c
    -> Widget m v p s p' s' a2 b2 c
mapHandler f (Widget (bld, hdl, act, disp)) = Widget
                   ( bld
                   , f hdl
                   , act
                   , disp)

mapActivator
    :: (F.Activator m v s c1 -> F.Activator m v s c2)
    -> Widget m v p s p' s' a b c1
    -> Widget m v p s p' s' a b c2
mapActivator f (Widget (bld, hdl, act, disp)) = Widget
                   ( bld
                   , hdl
                   , f act
                   , disp)

mapDisplay
    :: (F.Display m s () -> F.Display m s ())
    -> Widget m v p s p' s' a b c
    -> Widget m v p s p' s' a b c
mapDisplay f (Widget (bld, hdl, act, disp)) = Widget
                   ( bld
                   , hdl
                   , act
                   , f disp)

------------------------------------------

newtype WidgetModeller m v p p' s' a b c s = WidgetModeller
    { runWidgetModeller :: Widget m v p s p' s' a b c
    }

type instance F.Modeller (WidgetModeller m v p p' s' a b c) s = Widget m v p s p' s' a b c

instance F.ViaModel (WidgetModeller m v p p' s' a b c) where
    viaModel l (Widget (bld, hdl, act, disp)) = Widget
                   ( F.viaModel l bld
                   , F.viaModel l hdl
                   , F.viaModel l act
                   , F.viaModel l disp
                   )

------------------------------------------

newtype WidgetPlanner m v s p' s' a b c p = WidgetPlanner
    { runWidgetPlanner :: Widget m v p s p' s' a b c
    }

type instance F.Planner (WidgetPlanner m v s p' s' a b c) p = Widget m v p s p' s' a b c

instance F.ViaPlan (WidgetPlanner m v s p' s' a b c) where
    viaPlan l (Widget (bld, hdl, act, disp)) = Widget
                   ( F.viaPlan l bld
                   , hdl
                   , act
                   , disp
                   )

