{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Control.Monad.Trans.Cont
import Data.Generics.Product
import Data.IORef
import qualified Data.JSString as J
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Builder as R
import qualified Glazier.React.Framework.Core.Display as R
import qualified Glazier.React.Framework.Core.Finalizer as R
import qualified Glazier.React.Framework.Core.Handler as R
import qualified Glazier.React.Framework.Core.Initializer as R
import qualified Glazier.React.Framework.Core.Model as R
import qualified Glazier.React.Framework.Core.Obj as R
import qualified Glazier.React.Framework.Core.Prototype as R
import qualified JavaScript.Extras as JE

data Archetype m s c = Archetype
    { display' :: R.Display m s ()
    , finalizer' :: R.Finalizer m s
    , initializer' :: R.Initializer m s c
    } deriving (G.Generic, Functor)

-- mapBuilder' :: (R.Builder m i1 s i1 s -> R.Builder m i2 s i2 s)
--     -> Archetype m i1 s c -> Archetype m i2 s c
-- mapBuilder' f p = let bld = builder' p in p { builder' = f bld }
-- infixl 4 `mapBuilder'` -- like <$>

modifyDisplay' :: (R.Display m s () -> R.Display m s ())
    -> Archetype m s c -> Archetype m s c
modifyDisplay' f p = let disp = display' p in p { display' = f disp }
infixl 4 `modifyDisplay'` -- like <$>

modifyFinalizer' :: (R.Finalizer m s -> R.Finalizer m s)
    -> Archetype m s c -> Archetype m s c
modifyFinalizer' f p = let fin = finalizer' p in p { finalizer' = f fin }
infixl 4 `modifyFinalizer'` -- like <$>

modifyInitializer' :: (R.Initializer m s c1 -> R.Initializer m s c2)
    -> Archetype m s c1 -> Archetype m s c2
modifyInitializer' f p = let ini = initializer' p in p { initializer' = f ini }
infixl 4 `modifyInitializer'` -- like <$>

-- mapHandler' :: (R.Handler m s a1 b1 -> R.Handler m s a2 b2)
--     -> Archetype m i s c a1 b1 -> Archetype m i s c a2 b2
-- mapHandler' f p = let hdl = handler' p in p { handler' = f hdl }
-- infixl 4 `mapHandler'` -- like <$>

toArchetypeBuilder :: R.MonadReactor m
    => J.JSString
    -> R.Builder m i s i' s'
    -> R.Builder m i (IORef (R.Frame m s)) i' (IORef (R.Frame m s'))
toArchetypeBuilder n (R.Builder (R.MkReq mkReq, R.MkSpec mkSpc)) = R.Builder
        ( R.MkReq (R.doReadIORef >=> (mkReq . snd))
        , R.MkSpec $ \i -> do
            -- tuple the original state with a ComponentPlan
            -- and wrap inside a IORef
            s <- mkSpc i
            cp <- R.mkPlan n
            R.doNewIORef (cp, s)
        )

toArchetypeHandler ::
    R.SceneHandler m (R.Frame m s) s a b -- ^ @v@ is no longer polymorphic
    -> R.Handler m (IORef (R.Frame m s)) a b
toArchetypeHandler hdl ref = hdl (R.Obj ref id)

fromArchetypeHandler :: R.MonadReactor m => R.Handler m s a b -> R.SceneHandler m v s a b
fromArchetypeHandler hdl (R.Obj ref its) a = do
    obj <- lift $ R.doReadIORef ref
    hdl (obj ^. its.R.model) a

-- | NB. fromArchetype . toArchetype != id
toArchetype :: R.MonadReactor m
    => R.Prototype m (R.Frame m s) s c -- ^ @v@ is no longer polymorphic
    -> Archetype m (IORef (R.Frame m s)) c
toArchetype
    (R.Prototype dis fin ini)
    = Archetype dis' fin' ini'
  where
    dis' ref = do
        (cp, _) <- lift $ R.doReadIORef ref
        R.leaf
            (JE.justSnds [ ("updated", cp ^. field @"onUpdated")])
            (cp ^. field @"component".to JE.toJSR)
            (JE.justSnds
                [ ("key", Just . JE.toJSR $ cp ^. field @"reactKey")
                , ("render", JE.toJSR <$> cp ^. field @"onRender")
                ])
    fin' ref = do
        (cp, s) <- R.doReadIORef ref
        fin'' <- fin s
        pure (fin'' <> R.disposeOnRemoved cp <> R.disposeOnUpdated cp)
    ini' ref = do
        -- Run the Prototype's Initializer, saving the continuation result
        a <- ini (R.Obj ref id)
        -- Now activate this archetype, passing the output
        fmap (const a) . lift $ do
            -- Now add our own Archetype activation
            (cp, _) <- R.doReadIORef ref
            -- now replace the render and componentUpdated in the model if not already activated
            rnd <- case R.onRender cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.doMkRenderer $ do
                            s <- lift $ R.doReadIORef ref
                            dis s
            upd <- case R.onUpdated cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.doMkCallback (const $ pure ()) . const $ do
                            (cp', _) <- R.doReadIORef ref
                            R.doModifyIORef' ref $ \s -> s
                                -- can't use '.~' with afterOnUpdated - causes type inference errors
                                & (R.plan.field @"onceOnUpdated") `set'` pure mempty
                                & R.plan.field @"disposeOnUpdated" .~ mempty
                            -- Now run things on every updated
                            R.doDispose (R.disposeOnUpdated cp')
                            R.everyOnUpdated cp'
                            R.onceOnUpdated cp'
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

    -- hdl' ref = hdl (R.Obj ref id)

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor m
    => Archetype m s c
    -> R.Prototype m v s c
fromArchetype (Archetype disp fin ini) = R.Prototype
    (\(_, s) -> disp s)
    fin
    (\(R.Obj ref its) -> do
        obj <- lift $ R.doReadIORef ref
        ini (obj ^. its.R.model))
    -- (\(R.Obj ref its) a -> do
    --     obj <- lift $ R.doReadIORef ref
    --     hdl (obj ^. its.R.model) a)

fromArchetypeMaybe :: R.MonadReactor m
    => Archetype m s c
    -> R.Prototype m v (Maybe s) c
fromArchetypeMaybe (Archetype disp fin ini) = R.Prototype
    (\(_, s) -> maybe (pure ()) disp s)
    (maybe (pure mempty) fin)
    (\(R.Obj ref its) -> do
        obj <- lift $ R.doReadIORef ref
        case obj ^. its.R.model of
            Nothing -> ContT $ const $ pure ()
            Just s' -> ini s')
    -- (\(R.Obj ref its) a -> do
    --     obj <- lift $ R.doReadIORef ref
    --     hdl (obj ^. its.R.model) a)

fromArchetypeMaybeHandler :: R.MonadReactor m => R.Handler m s a b -> R.SceneHandler m v (Maybe s) a b
fromArchetypeMaybeHandler hdl (R.Obj ref its) a = do
    obj <- lift $ R.doReadIORef ref
    case obj ^. its.R.model of
        Nothing -> ContT $ const $ pure ()
        Just s' -> hdl s' a
