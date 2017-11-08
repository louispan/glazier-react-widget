{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Widget where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
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
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

newtype Prototype m v p s p' s' a b c = Prototype {
    runPrototype ::
           ( F.Builder m p s p' s'
           , F.Handler' m v s a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.Activator' m v s c
           , F.Display m s ()
           )
    }

-- instance R.MonadReactor m =>
--          F.IORefModel
--              (Prototype m s p s p' s' a b c)
--              (Prototype m v p (IORef s) p' (IORef s') a b c) where
--     ioRefModel (Prototype (bld, hdl, act, disp)) = Prototype
--         ( F.ioRefModel bld
--         , F.ioRefModel hdl
--         , F.ioRefModel act
--         , F.ioRefModel disp
--         )

------------------------------------------

newtype PPrototype m v p s psabc = PPrototype {
    runPPrototype :: Prototype m v p s (P.At0 psabc) (P.At1 psabc) (P.At2 psabc) (P.At3 psabc) (P.At4 psabc)
    }

type instance P.PNullary (PPrototype m v p s) (p', s', a, b, c) = Prototype m v p s p' s' a b c

instance Monad m => P.PMEmpty (PPrototype m p s v) (Many '[], Many '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
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
         ) => P.PSemigroup (PPrototype m v p s)
             (Many p1, Many s1, Which a1, Which b1, Which c1)
             (Many p2, Many s2, Which a2, Which b2, Which c2)
             (Many p3, Many s3, Which a3, Which b3, Which c3) where
    (Prototype (bld1, hdl1, act1, disp1)) `pmappend` (Prototype (bld2, hdl2, act2, disp2)) =
        Prototype
        ( bld1 `P.pmappend` bld2
        , hdl1 `P.pmappend` hdl2
        , act1 `P.pmappend` act2
        , disp1 <> disp2
        )

------------------------------------------

building
    :: Monad m
    => F.Builder m p s p' s'
    -> Prototype m v p s p' s' (Which '[]) (Which '[]) (Which '[])
building bld = Prototype
        ( bld
        , P.pmempty
        , P.pmempty
        , mempty)

handling
    :: Monad m
    => F.Handler' m v s a b
    -> Prototype m v p s (Many '[]) (Many '[]) a b (Which '[])
handling hdl = Prototype
        ( P.pmempty
        , hdl
        , P.pmempty
        , mempty)

activating
    :: Monad m
    => F.Activator' m v s c
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) c
activating act = Prototype
        ( P.pmempty
        , P.pmempty
        , act
        , mempty)

displaying
    :: Applicative m
    => F.Display m s ()
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) (Which '[])
displaying d = Prototype
        ( P.pmempty
        , P.pmempty
        , P.pmempty
        , d)

------------------------------------------

mapBuilder
    :: (F.Builder m p1 s p1' s1' -> F.Builder m p2 s p2' s2')
    -> Prototype m v p1 s p1' s1' a b c
    -> Prototype m v p2 s p2' s2' a b c
mapBuilder f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( f bld
                   , hdl
                   , act
                   , disp)

mapHandler
    :: (F.Handler' m v s a1 b1 -> F.Handler' m v s a2 b2)
    -> Prototype m v p s p' s' a1 b1 c
    -> Prototype m v p s p' s' a2 b2 c
mapHandler f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , f hdl
                   , act
                   , disp)

mapActivator
    :: (F.Activator' m v s c1 -> F.Activator' m v s c2)
    -> Prototype m v p s p' s' a b c1
    -> Prototype m v p s p' s' a b c2
mapActivator f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , hdl
                   , f act
                   , disp)

mapDisplay
    :: (F.Display m s () -> F.Display m s ())
    -> Prototype m v p s p' s' a b c
    -> Prototype m v p s p' s' a b c
mapDisplay f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , hdl
                   , act
                   , f disp)

------------------------------------------

newtype PrototypeModeller m v p p' s' a b c s = PrototypeModeller
    { runPrototypeModeller :: Prototype m v p s p' s' a b c
    }

type instance F.Modeller (PrototypeModeller m v p p' s' a b c) s = Prototype m v p s p' s' a b c

instance F.ViaModel (PrototypeModeller m v p p' s' a b c) where
    viaModel l (Prototype (bld, hdl, act, disp)) = Prototype
                   ( F.viaModel l bld
                   , F.viaModel l hdl
                   , F.viaModel l act
                   , F.viaModel l disp
                   )

------------------------------------------

newtype PrototypePlanner m v s p' s' a b c p = PrototypePlanner
    { runPrototypePlanner :: Prototype m v p s p' s' a b c
    }

type instance F.Planner (PrototypePlanner m v s p' s' a b c) p = Prototype m v p s p' s' a b c

instance F.ViaPlan (PrototypePlanner m v s p' s' a b c) where
    viaPlan l (Prototype (bld, hdl, act, disp)) = Prototype
                   ( F.viaPlan l bld
                   , hdl
                   , act
                   , disp
                   )

newtype Archetype m p s a b c = Archetype {
    runArchetype ::
           ( F.Builder m p s p s
           , F.Handler m s a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.Activator m s c
           , F.Display m s ()
           )
    }

-- type Widget = (F.ComponentModel, 


-- | FIXME: add component triggers, etc
archetype :: R.MonadReactor m
    => Prototype m (F.ComponentModel, s) p s p s a b c
    -> Archetype m p (IORef (F.ComponentModel, s)) a b c
archetype (Prototype ( F.Builder (F.MkPlan mkPlan, F.MkModel mkModel)
                     , F.Handler hdl
                     , F.Activator act
                     , F.Display disp)) = Archetype
     ( F.Builder ( F.MkPlan (R.doReadIORef >=> (mkPlan . snd))
                 , F.MkModel $ \p -> do
                         s <- mkModel p
                         -- create a model with a dummy render for now
                         cm <- F.ComponentModel
                                 <$> R.getComponent
                                 <*> R.mkKey
                                 <*> pure (R.Renderer (J.Callback J.nullRef))
                                 <*> pure []
                         ref <- R.doNewIORef (cm, s)
                         -- now replace the render in the model
                         rnd <- R.mkRenderer $ do
                             (_, s') <- lift $ R.doReadIORef ref
                             disp s'
                         R.doModifyIORef' ref (\(cm', s') -> (cm' & F.componentRender .~ rnd, s'))
                         pure ref
                 )
     , F.Handler $ \ref a -> hdl (ref, Lens _2) a
     , F.Activator $ \ref exec -> act (ref, Lens _2) exec
     , F.Display $ \ref -> do
             (cm, _) <- lift $ R.doReadIORef ref
             R.lf (cm ^. F.component . to JE.toJS')
                 (cm ^. F.componentListeners)
                 [ ("key",  cm ^. F.componentKey . to (JE.toJS' . R.runKey))
                 , ("render", cm ^. F.componentRender . to (JE.toJS' . R.runRenderer))
                 ]
     )
