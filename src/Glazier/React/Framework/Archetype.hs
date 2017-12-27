{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Archetype where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Generics.Product
import qualified Data.JSString as JS
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified JavaScript.Extras as JE

newtype Archetype m i s x c a b = Archetype {
    runArchetype ::
           ( F.Display m s ()
           , F.Builder m i s i s
           , F.Executor m s x c a b
           )
    }

-- | NB. fromArchetype . toArchetype != id
toArchetype :: (R.MonadReactor x m, AsFacet (CD.Disposable ()) x)
    => JS.JSString -> F.Prototype m (F.ComponentPlan, s) i s i s x c a b
    -> Archetype m i (IORef (F.ComponentPlan, s)) x c a b
toArchetype n (F.Prototype ( F.Display disp
                         , F.Builder (F.MkInfo mkInf, F.MkModel mkMdl)
                         , F.Executor exec
                         )) = Archetype
     ( F.Display $ \ref -> do
             (cp, _) <- lift $ R.doReadIORef ref
             R.lf (cp ^. field @"component".to JE.toJS')
                 (JE.justSnds $
                     [ ("updated", cp ^. field @"onUpdated")
                     ]
                 )
                 (JE.justSnds $
                     [ ("key", Just . JE.toJS' $ cp ^. field @"key")
                     , ("render", JE.toJS' <$> cp ^. field @"onRender")
                     ]
                 )
     , F.Builder ( F.MkInfo (R.doReadIORef >=> (mkInf . snd))
                 , F.MkModel $ \i -> do
                         -- tuple the original state with a ComponentPlan
                         -- and wrap inside a IORef
                         s <- mkMdl i
                         -- create a ComponentPlan with no callbackss
                         cp <- F.ComponentPlan
                                 <$> R.getComponent
                                 <*> pure mempty -- Disposables
                                 <*> R.mkReactKey n
                                 <*> pure Nothing -- render
                                 <*> pure Nothing -- callback
                                 <*> pure 0
                         -- create the IORef
                         R.doNewIORef (cp, s)
                 )
     , F.Executor $ \k -> let (F.Activator act, F.Handler hdl) = exec k
                          in ( F.Activator $ \ref -> do
                              act (ref, Lens id)
                              (cp, _) <- R.doReadIORef ref
                              -- now replace the render and componentUpdated in the model if not already activated
                              rnd <- case cp ^. field @"onRender" of
                                         Just rnd' -> pure rnd'
                                         Nothing -> R.mkRenderer $ do
                                             s <- lift $ R.doReadIORef ref
                                             disp s
                              upd <- case cp ^. field @"onUpdated" of
                                         Just upd' -> pure upd'
                                         Nothing -> R.mkCallback (const $ pure ()) (const $ do
                                             (cp', _) <- R.doReadIORef ref
                                             let ds = cp' ^. field @"disposable"
                                             case CD.runDisposable ds of
                                                 Nothing -> pure DL.empty
                                                 Just _ -> do
                                                     R.doModifyIORef' ref (\(cp'', s') ->
                                                         (cp'' & field @"disposable" .~ mempty
                                                         , s'))
                                                     pure $ DL.singleton $ review facet ds)
                              R.doModifyIORef' ref (\(cp', s') ->
                                          ( cp' & field @"onRender" .~ (Just rnd)
                                                & field @"onUpdated" .~ (Just upd)
                                          , s'))

                             , F.Handler $ \ref a -> hdl (ref, Lens id) a
                             )
     )

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor x m => Archetype m i s x c a b -> F.Prototype m v i s i s x c a b
fromArchetype (Archetype ( F.Display disp
                         , bld
                         , F.Executor exec
                         -- , F.Handler hdl
                         -- , F.Activator act
                         )) = F.Prototype
    ( F.Display $ \(_, s) -> disp s
    , bld
    , F.Executor $ \k -> let (F.Activator act, F.Handler hdl) = exec k
                          in ( F.Activator $ \(ref, Lens this) -> do
                                     obj <- R.doReadIORef ref
                                     act (obj ^. this._2)
                             , F.Handler $ \(ref, Lens this) a -> do
                                     obj <- R.doReadIORef ref
                                     hdl (obj ^. this._2) a
                             )
    )
