{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Archetype where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.DList as DL
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

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

-- | NB. fromArchetype . toArchetype != id
toArchetype :: (R.MonadReactor m, F.HasReactKey s, F.HasComponentListeners s)
    => F.Prototype m (F.ComponentModel, s) p s p s a b c
    -> Archetype m p (IORef (F.ComponentModel, s)) a b c
toArchetype (F.Prototype ( F.Builder (F.MkPlan mkPlan, F.MkModel mkModel)
                     , F.Handler hdl
                     , F.Activator act
                     , F.Display disp)) = Archetype
     ( F.Builder ( F.MkPlan (R.doReadIORef >=> (mkPlan . snd))
                 , F.MkModel $ \p -> do
                         -- tuple the original state with a ComponentModel
                         -- and wrap inside a IORef
                         s <- mkModel p
                         -- create a ComponentModel with a dummy render for now
                         cm <- F.ComponentModel
                                 <$> R.getComponent
                                 <*> pure (R.Renderer (J.Callback J.nullRef))
                         -- create the IORef
                         ref <- R.doNewIORef (cm, s)
                         -- now replace the render in the model
                         rnd <- R.mkRenderer $ do
                             (_, s') <- lift $ R.doReadIORef ref
                             disp s'
                         R.doModifyIORef' ref (\(cm', s') -> (cm' & F.componentRender .~ rnd, s'))
                         -- return the ioref
                         pure ref
                 )
     , F.Handler $ \ref a -> hdl (ref, Lens _2) a
     , F.Activator $ \ref exec -> act (ref, Lens _2) exec
     , F.Display $ \ref -> do
             (cm, s) <- lift $ R.doReadIORef ref
             R.lf (cm ^. F.component . to JE.toJS')
                 (s ^. F.componentListeners . to (DL.toList . F.runComponentListeners))
                 [ ("key",  s ^. F.reactKey . to (JE.toJS' . R.runReactKey))
                 , ("render", cm ^. F.componentRender . to (JE.toJS' . R.runRenderer))
                 ]
     )

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor m => Archetype m p s a b c -> F.Prototype m v p s p s a b c
fromArchetype (Archetype ( bld
                     , F.Handler hdl
                     , F.Activator act
                     , disp)) = F.Prototype
    ( bld
    , F.Handler $ \(ref, Lens this) a -> do
            obj <- R.doReadIORef ref
            hdl (obj ^. this) a
    , F.Activator $ \(ref, Lens this) exec -> do
            obj <- R.doReadIORef ref
            act (obj ^. this) exec
    , disp )


mapBuilder2
    :: (F.Builder m p s p s -> F.Builder m p s p s)
    -> Archetype m p s a b c
    -> Archetype m p s a b c
mapBuilder2 f (Archetype (bld, hdl, act, disp)) = Archetype
                   ( f bld
                   , hdl
                   , act
                   , disp)

mapHandler2
    :: (F.Handler m s a1 b1 -> F.Handler m s a2 b2)
    -> Archetype m p s a1 b1 c
    -> Archetype m p s a2 b2 c
mapHandler2 f (Archetype (bld, hdl, act, disp)) = Archetype
                   ( bld
                   , f hdl
                   , act
                   , disp)

mapActivator2
    :: (F.Activator m s c1 -> F.Activator m s c2)
    -> Archetype m p s a b c1
    -> Archetype m p s a b c2
mapActivator2 f (Archetype (bld, hdl, act, disp)) = Archetype
                   ( bld
                   , hdl
                   , f act
                   , disp)

mapDisplay2
    :: (F.Display m s () -> F.Display m s ())
    -> Archetype m p s a b c
    -> Archetype m p s a b c
mapDisplay2 f (Archetype (bld, hdl, act, disp)) = Archetype
                   ( bld
                   , hdl
                   , act
                   , f disp)
