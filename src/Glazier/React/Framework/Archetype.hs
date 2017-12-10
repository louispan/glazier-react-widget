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
toArchetype :: R.MonadReactor m
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
                         -- create a ComponentModel with a dummy render and updated for now
                         cm <- F.ComponentModel
                                 <$> R.getComponent
                                 <*> pure (pure ()) -- Disposable
                                 <*> pure (J.Callback J.nullRef)
                                 <*> R.mkReactKey
                                 <*> pure (R.Renderer (J.Callback J.nullRef))
                         -- create the IORef
                         ref <- R.doNewIORef (cm, s)
                         -- now replace the render in the model
                         rnd <- R.mkRenderer $ do
                             (cm', s') <- lift $ R.doReadIORef ref
                             disp (cm' ^. F.componentKey, s')
                         upd <- R.mkCallback (const $ pure ()) (const $ do
                                 (cm', _) <- readIORef ref
                                 let d = cm' ^. F.componentDisposable
                                 modifyIORef' ref (\(cm'', s') ->
                                     (cm'' & F.componentDisposable .~ (pure ()), s'))
                                 R.runDisposable d)
                         R.doModifyIORef' ref (\(cm', s') ->
                                     ( cm' & F.componentRender .~ rnd
                                           & F.componentUpdated .~ upd
                                     , s'))
                         -- return the ioref
                         pure ref
                 )
     , F.Handler $ \ref a -> hdl (ref, Lens id) a
     , F.Activator $ \ref exec -> act (ref, Lens id) exec
     , F.Display $ \ref -> do
             (cm, _) <- lift $ R.doReadIORef ref
             R.lf (cm ^. F.component.to JE.toJS')
                 mempty
                 [ ("key",  cm ^. F.componentKey.to JE.toJS')
                 , ("render", cm ^. F.componentRender.to JE.toJS')
                 , ("render", cm ^. F.componentRender.to JE.toJS')
                 ]
     )

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor m => Archetype m p s a b c -> F.Prototype m v p s p s a b c
fromArchetype (Archetype ( bld
                     , F.Handler hdl
                     , F.Activator act
                     , F.Display disp)) = F.Prototype
    ( bld
    , F.Handler $ \(ref, Lens this) a -> do
            obj <- R.doReadIORef ref
            hdl (obj ^. this._2) a
    , F.Activator $ \(ref, Lens this) exec -> do
            obj <- R.doReadIORef ref
            act (obj ^. this._2) exec
    , F.Display $ \(_, s) -> disp s)


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
