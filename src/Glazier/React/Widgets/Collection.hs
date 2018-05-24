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

-- | Add a constructed subject to a parent widget
addSubject :: (MonadReactor p ss cmd m)
    => Widget cmd s s a
    -> s
    -> (Subject s -> ss -> ss)
    -> m a
addSubject wid s f = mkSubject wid s $ \sbj ->
    tickScene (_model %= f sbj)

-- | Given a Foldable of items that have been removed from a Collection model
-- schedule cleanup of the callbacks when the parent widget is rerendered.
cleanupSubjects ::
    (MonadReactor p ss cmd m, Foldable t)
    => t (Subject s) -> m ()
cleanupSubjects ss = do
    sbj <- view _subject
    let cleanup = foldr ((>>) . prolong) (pure ()) ss
    postCmd' . TickScene sbj $ _plan._doOnRendered._once %= (*> cleanup)

-- | Collection doesn't have an initializing gadget since
-- the 'Subject's in the model are all initialized via 'addSubject'.
collectionDisplay :: (Functor t, Foldable t)
    => Window (t (Subject s)) ()
collectionDisplay = do
    ss <- view _model
    let toLi s = branch "li" [] (displaySubject s)
    branch "ul" [] (fold $ toLi <$> ss)


-- collectionWidget :: (Foldable t, AsReactor cmd)
--     => Widget cmd s s a -> Widget cmd p (t (Subject s)) a
-- collectionWidget (Widget win gad) = Widget
