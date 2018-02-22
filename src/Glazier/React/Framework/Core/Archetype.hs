{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Archetype where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Generics.Product
import Data.IORef
import qualified Data.JSString as J
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Activator as F
import qualified Glazier.React.Framework.Core.Builder as F
import qualified Glazier.React.Framework.Core.Display as F
import qualified Glazier.React.Framework.Core.Finalizer as F
import qualified Glazier.React.Framework.Core.Handler as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F
import qualified Glazier.React.Framework.Core.Prototype as F
import qualified JavaScript.Extras as JE

data Archetype m i s c a b = Archetype
    { builder' :: F.Builder m i s i s
    , display' :: F.Display m s ()
    , finalizer' :: F.Finalizer m s
    , activator' :: F.Activator m s c
    , handler' :: F.Handler m s a b
    } deriving (G.Generic)

mapBuilder' :: (F.Builder m i1 s i1 s -> F.Builder m i2 s i2 s)
    -> Archetype m i1 s c a b -> Archetype m i2 s c a b
mapBuilder' f p = let bld = builder' p in p { builder' = f bld }
infixl 4 `mapBuilder'` -- like <$>

mapDisplay' :: (F.Display m s () -> F.Display m s ())
    -> Archetype m i s c a b -> Archetype m i s c a b
mapDisplay' f p = let disp = display' p in p { display' = f disp }
infixl 4 `mapDisplay'` -- like <$>

mapFinalizer' :: (F.Finalizer m s -> F.Finalizer m s)
    -> Archetype m i s c a b -> Archetype m i s c a b
mapFinalizer' f p = let fin = finalizer' p in p { finalizer' = f fin }
infixl 4 `mapFinalizer'` -- like <$>

mapActivator' :: (F.Activator m s c1 -> F.Activator m s c2)
    -> Archetype m i s c1 a b -> Archetype m i s c2 a b
mapActivator' f p = let act = activator' p in p { activator' = f act }
infixl 4 `mapActivator'` -- like <$>

mapHandler' :: (F.Handler m s a1 b1 -> F.Handler m s a2 b2)
    -> Archetype m i s c a1 b1 -> Archetype m i s c a2 b2
mapHandler' f p = let hdl = handler' p in p { handler' = f hdl }
infixl 4 `mapHandler'` -- like <$>

-- | NB. fromArchetype . toArchetype != id
toArchetype :: R.MonadReactor m
    => J.JSString
    -> F.Prototype m (F.Frame m s) i s i s c a b
    -> Archetype m i (IORef (F.Frame m s)) c a b
toArchetype n (F.Prototype
    (F.Builder (F.MkInfo mkInf, F.MkSpec mkSpc))
    dis
    fin
    act
    hdl
    )
    = Archetype bld' dis' fin' act' hdl'
  where
    bld' = F.Builder
        ( F.MkInfo (R.doReadIORef >=> (mkInf . snd))
        , F.MkSpec $ \i -> do
            -- tuple the original state with a ComponentPlan
            -- and wrap inside a IORef
            s <- mkSpc i
            cp <- F.mkPlan n
            R.doNewIORef (cp, s))
    dis' ref = do
        (cp, _) <- lift $ R.doReadIORef ref
        R.leaf (cp ^. field @"component".to JE.toJS')
            (JE.justSnds
                [ ("updated", cp ^. field @"onUpdated")
                ]
            )
            (JE.justSnds
                [ ("key", Just . JE.toJS' $ cp ^. field @"reactKey")
                , ("render", JE.toJS' <$> cp ^. field @"onRender")
                ])
    fin' ref = do
        (cp, s) <- R.doReadIORef ref
        fin'' <- fin s
        pure (fin'' <> F.disposeOnRemoved cp <> F.disposeOnUpdated cp)
    act' ref = do
        -- Run the Prototype's activator, saving the continuation result
        a <- act (F.Obj ref id)
        -- Now activate this archetype, passing the output
        fmap (const a) . lift $ do
            -- Now add our own Archetype activation
            (cp, _) <- R.doReadIORef ref
            -- now replace the render and componentUpdated in the model if not already activated
            rnd <- case F.onRender cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.doMkRenderer $ do
                            s <- lift $ R.doReadIORef ref
                            dis s
            upd <- case F.onUpdated cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.doMkCallback (const $ pure ()) . const $ do
                            (cp', _) <- R.doReadIORef ref
                            R.doModifyIORef' ref $ \s -> s
                                -- can't use '.~' with afterOnUpdated - causes type inference errors
                                & (F.plan.field @"afterOnUpdated") `set'` pure mempty
                                & F.plan.field @"disposeOnUpdated" .~ mempty
                            R.doDispose (F.disposeOnUpdated cp')
                            F.afterOnUpdated cp'
            let rnd' = (\(d, cb) cp' -> cp' & field @"onRender" .~ Just cb
                                            & field @"disposeOnRemoved" %~ (<> d)
                        ) <$> rnd
                upd' = (\(d, cb) cp' -> cp' & field @"onUpdated" .~ Just cb
                                            & field @"disposeOnRemoved" %~ (<> d)
                        ) <$> upd
                mf = case (rnd', upd') of
                        (Nothing, x) -> x
                        (x, Nothing) -> x
                        (Just x, Just y) -> Just (y . x)
            case mf of
                Nothing -> pure ()
                Just g -> R.doModifyIORef' ref (first g)
    hdl' ref = hdl (F.Obj ref id)

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor m
    => Archetype m i s c a b
    -> F.Prototype m v i s i s c a b
fromArchetype (Archetype
    bld
    dis
    fin
    act
    hdl
    )
    = F.Prototype
    bld
    (\(_, s) -> dis s)
    fin
    (\(F.Obj ref its) -> do
        obj <- lift $ R.doReadIORef ref
        act (obj ^. its.F.model))
    (\(F.Obj ref its) a -> do
        obj <- lift $ R.doReadIORef ref
        hdl (obj ^. its.F.model) a)
