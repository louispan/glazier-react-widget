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
import qualified Data.JSString as J
import Data.IORef
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Disposer as F
import qualified Glazier.React.Framework.Window as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified JavaScript.Extras as JE

newtype Archetype m i s x c a b = Archetype {
    runArchetype ::
           ( F.Disposer m s
           , F.Window m s ()
           , F.Builder m i s i s
           , F.Executor m s x c a b
           )
    }

-- | NB. fromArchetype . toArchetype != id
toArchetype :: (R.MonadReactor x m, AsFacet CD.Disposable x)
    => J.JSString -> F.Prototype m (F.ComponentPlan x m, s) i s i s x c a b
    -> Archetype m i (IORef (F.ComponentPlan x m, s)) x c a b
toArchetype n (F.Prototype ( F.Disposer dis
                           , F.Window win
                           , F.Builder (F.MkInfo mkInf, F.MkModel mkMdl)
                           , F.Executor exec
                           )) = Archetype
     ( F.Disposer $ \ref -> do
             (cp, s) <- R.doReadIORef ref
             dis' <- dis s
             pure (dis' <> (F.finalizer cp) <> (F.disposeOnUpdated cp))
     , F.Window $ \ref -> do
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
                                 <*> R.mkReactKey n
                                 <*> pure 0
                                 <*> pure mempty -- finalizer
                                 <*> pure mempty -- disposeOnUpdated
                                 <*> pure (pure mempty) -- doOnUpdated
                                 <*> pure Nothing -- callback
                                 <*> pure Nothing -- render
                         -- create the IORef
                         R.doNewIORef (cp, s)
                 )
     , F.Executor $ \k -> let (F.Activator act, F.Handler hdl) = exec k
                          in ( F.Activator $ \ref -> do
                              act (ref, Lens id)
                              (cp, _) <- R.doReadIORef ref
                              -- now replace the render and componentUpdated in the model if not already activated
                              rnd <- case F.onRender cp of
                                         Just _ -> pure Nothing
                                         Nothing -> fmap Just . R.mkRenderer $ do
                                             s <- lift $ R.doReadIORef ref
                                             win s
                              upd <- case F.onUpdated cp of
                                         Just _ -> pure Nothing
                                         Nothing -> fmap Just . R.mkCallback (const $ pure ()) . const $ do
                                             (cp', _) <- R.doReadIORef ref
                                             let d = F.disposeOnUpdated cp'
                                                 d' = DL.singleton $ review facet d
                                                 x = F.doOnUpdated cp'
                                             R.doModifyIORef' ref $ \(cp'', s') ->
                                                  (cp'' & field @"doOnUpdated" `set'` (pure mempty)
                                                        & field @"disposeOnUpdated" .~ mempty
                                                  , s')
                                             (<> d') <$> x
                              let rnd' = (\(d, cb) cp' -> cp' & field @"onRender" .~ (Just cb)
                                                              & field @"finalizer" %~ (<> d)
                                         ) <$> rnd
                                  upd' = (\(d, cb) cp' -> cp' & field @"onUpdated" .~ (Just cb)
                                                              & field @"finalizer" %~ (<> d)
                                         ) <$> upd
                                  mf = case (rnd', upd') of
                                          (Nothing, x) -> x
                                          (x, Nothing) -> x
                                          (Just x, Just y) -> Just (y . x)
                              case mf of
                                  Nothing -> pure ()
                                  Just g -> R.doModifyIORef' ref (\(cp', s') -> (g cp', s'))
                             , F.Handler $ \ref a -> hdl (ref, Lens id) a
                             )
     )

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor x m => Archetype m i s x c a b -> F.Prototype m v i s i s x c a b
fromArchetype (Archetype ( dis
                         , F.Window win
                         , bld
                         , F.Executor exec
                         )) = F.Prototype
    ( dis
    , F.Window $ \(_, s) -> win s
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
