{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Archetype where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Generics.Product
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

newtype Archetype m p s x a b y = Archetype {
    runArchetype ::
           ( F.Builder m p s p s
           , F.Handler m s x a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.Activator m s y
           , F.Display m s ()
           )
    }

-- | NB. fromArchetype . toArchetype != id
toArchetype :: R.MonadReactor m
    => F.Prototype m (F.ComponentModel, s) p s p s x a b y
    -> Archetype m p (IORef (F.ComponentModel, s)) x a b y
toArchetype (F.Prototype ( F.Builder (F.MkPlan mkPlan, F.MkModel mkModel)
                         , F.Handler hdl
                         , F.Activator act
                         , F.Display disp)) = Archetype
     ( F.Builder ( F.MkPlan (R.doReadIORef >=> (mkPlan . snd))
                 , F.MkModel $ \p -> do
                         -- tuple the original state with a ComponentModel
                         -- and wrap inside a IORef
                         s <- mkModel p
                         -- create a ComponentModel with a dummy render and updated for now
                         cm <- F.ComponentModel
                                 <$> R.getComponent
                                 <*> pure mempty -- Disposables
                                 <*> pure (J.Callback J.nullRef)
                                 <*> R.mkReactKey
                                 <*> pure (R.Renderer (J.Callback J.nullRef))
                                 <*> pure 0
                         -- create the IORef
                         ref <- R.doNewIORef (cm, s)
                         -- now replace the render in the model
                         rnd <- R.mkRenderer $ do
                             (_, s') <- lift $ R.doReadIORef ref
                             disp s'
                         upd <- R.mkCallback (const $ pure ()) (const $ do
                                 (cm', _) <- readIORef ref
                                 let ds = cm' ^. field @"componentDisposable"
                                 case R.runDisposable ds of
                                     Nothing -> pure ()
                                     Just ds' -> do
                                         modifyIORef' ref (\(cm'', s') ->
                                             (cm'' & field @"componentDisposable" .~ mempty
                                             , s'))
                                         ds')
                         R.doModifyIORef' ref (\(cm', s') ->
                                     ( cm' & field @"componentRender" .~ rnd
                                           & field @"componentUpdated" .~ upd
                                     , s'))
                         -- return the ioref
                         pure ref
                 )
     , F.Handler $ \exec ref a -> hdl exec (ref, Lens id) a
     , F.Activator $ \exec ref -> act exec (ref, Lens id)
     , F.Display $ \ref -> do
             (cm, _) <- lift $ R.doReadIORef ref
             R.lf (cm ^. field @"component".to JE.toJS')
                 mempty
                 [ ("key",  cm ^. field @"componentKey".to JE.toJS')
                 , ("render", cm ^. field @"componentRender".to JE.toJS')
                 , ("updated", cm ^. field @"componentUpdated".to JE.toJS')
                 ]
     )

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor m => Archetype m p s x a b y -> F.Prototype m v p s p s x a b y
fromArchetype (Archetype ( bld
                     , F.Handler hdl
                     , F.Activator act
                     , disp)) = F.Prototype
    ( bld
    , F.Handler $ \exec (ref, Lens this) a -> do
            obj <- R.doReadIORef ref
            hdl exec (obj ^. this._2) a
    , F.Activator $ \exec (ref, Lens this) -> do
            obj <- R.doReadIORef ref
            act exec (obj ^. this._2)
    , disp)


-- mapBuilder2
--     :: (F.Builder m p s p s -> F.Builder m p s p s)
--     -> Archetype m p s a b c
--     -> Archetype m p s a b c
-- mapBuilder2 f (Archetype (bld, hdl, act, disp)) = Archetype
--                    ( f bld
--                    , hdl
--                    , act
--                    , disp)

-- mapHandler2
--     :: (F.Handler m s a1 b1 -> F.Handler m s a2 b2)
--     -> Archetype m p s a1 b1 c
--     -> Archetype m p s a2 b2 c
-- mapHandler2 f (Archetype (bld, hdl, act, disp)) = Archetype
--                    ( bld
--                    , f hdl
--                    , act
--                    , disp)

-- mapActivator2
--     :: (F.Activator m s c1 -> F.Activator m s c2)
--     -> Archetype m p s a b c1
--     -> Archetype m p s a b c2
-- mapActivator2 f (Archetype (bld, hdl, act, disp)) = Archetype
--                    ( bld
--                    , hdl
--                    , f act
--                    , disp)

-- mapDisplay2
--     :: (F.Display m s () -> F.Display m s ())
--     -> Archetype m p s a b c
--     -> Archetype m p s a b c
-- mapDisplay2 f (Archetype (bld, hdl, act, disp)) = Archetype
--                    ( bld
--                    , hdl
--                    , act
--                    , f disp)
