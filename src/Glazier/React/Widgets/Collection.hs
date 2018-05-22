{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widgets.Collection where

import Control.Lens
import Data.Foldable
-- import qualified Data.Foldable.Esoteric as E
import Glazier.React

-- | Convert a gadget for a widget into a gadget for a list of widgets
collectionGadget :: (Foldable t, AsReactor cmd)
    => Gadget cmd s s a -> Gadget cmd p (t (Subject s)) a
collectionGadget gad =
    -- Given a handler for @a@ event
    delegate $ \fire -> getScene $ \scn ->
        -- | run the original gadget for each of the Subject s in the model
        foldMap (\sbj ->
            (lift $ runAReaderT gad $ Entity sbj id) >>= fire) (view _model scn)

-- Given a Foldable of items that have been removed from a Collection model
-- schedule cleanup of the callbacks
collectionCleanupOnRendered ::
    (MonadReactor p s cmd m, Foldable t)
    => t (Subject s) -> m ()
collectionCleanupOnRendered ss = do
    sbj <- view _subject
    let cleanup = foldr ((>>) . prolong) (pure ()) ss
    postCmd' . TickScene sbj $ _plan._doOnRendered._once %= (*> cleanup)

collectionDisplay :: (Functor t, Foldable t)
    => Window (t (Subject s)) ()
collectionDisplay = do
    ss <- view _model
    let toLi s = branch "li" [] (displaySubject s)
    branch "ul" [] (fold $ toLi <$> ss)

-- collectionWidget :: (Foldable t, AsReactor cmd)
--     => Widget cmd s s a -> Widget cmd p (t (Subject s)) a
-- collectionWidget (Widget win gad) = Widget
