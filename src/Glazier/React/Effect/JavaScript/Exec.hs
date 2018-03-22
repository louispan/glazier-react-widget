{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.JavaScript.Exec where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React.Effect.JavaScript
import Glazier.React.Framework
import qualified JavaScript.Extras as JE

execSetProperty ::
    ( MonadIO m
    )
    => SetProperty -> m ()
execSetProperty (SetProperty prop j) = liftIO $ JE.setProperty prop j

execGetProperty ::
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (Scene x s)) r
    , HasItem' (TVar (Scene x s)) r
    )
    => (DL.DList x -> m ())
    -> GetProperty' (Scene x s)
    -> m ()
execGetProperty exec (GetProperty n j k) = do
    world <- view item' <$> ask
    frame <- view item' <$> ask
    r <- liftIO $ JE.getProperty n j
    -- Apply to result to the world state, and execute any produced commands
    xs <- liftIO $ atomically $ runAction world frame (k r)
    exec xs
