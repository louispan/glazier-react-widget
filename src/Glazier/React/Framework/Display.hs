{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Glazier.React.Framework.Display where

import Control.Lens
import Control.Monad.Trans.Class
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F

newtype Display m s a = Display
    { runDisplay :: s -> R.ReactMlT m a
    } deriving (Functor)

instance Monad m => Applicative (Display m s) where
    pure = Display . const . pure
    Display f <*> Display g = Display $ \s -> f s <*> g s

instance Monad m => Monad (Display m s) where
    Display f >>= k = Display $ \s -> f s >>= k' s
      where
        k' s = (`runDisplay` s) . k

newtype DisplayModeller m a s = DisplayModeller { runDisplayModeller :: Display m s a }

instance F.Modeller (Display m s a) (DisplayModeller m a) s where
    toModeller = DisplayModeller
    fromModeller = runDisplayModeller

instance R.MonadReactor m => F.ViaModel (DisplayModeller m a) where
    viaModel l (DisplayModeller (Display f)) = DisplayModeller $ Display $ f . view l

instance R.MonadReactor m => F.IORefModel (Display m s a) (Display m (IORef s) a) where
    ioRefModel (Display disp) = Display $ \ref -> lift (R.doReadIORef ref) >>= disp

-- -- | Add a list of static properties to the rendered element.
-- decorate :: [JE.Property] -> Display m specs
-- decorate ps = Display (mempty, const ps, Nothing)

-- display :: Window m specs -> Display m specs
-- display w = Display (mempty, mempty, Just w)

-- -- | Create a display that also get the key and properties from the specs
-- widgetDisplay :: ( UniqueMember [R.Listener] specs
--                  , UniqueMember F.Key specs
--                  , UniqueMember [JE.Property] specs) => Display m specs
-- widgetDisplay = Display (ls, ps, Nothing)
--   where
--     ls s = s ^. item
--     p s = s ^. item
--     k s = s ^. item
--     ps s = ("key", JE.toJS' . F.runKey $ k s) : p s

-- -- | wrap with a div iff there are properties and listeners
-- divIfNeeded :: Monad m => (Many specs -> R.ReactMlT m ()) -> Display m specs
-- divIfNeeded w = Display (mempty, mempty, Just go)
--   where
--     go l p s = do
--         let p' = p s
--             l' = l s
--         case (l', p') of
--             ([], []) -> w s
--             _ -> R.bh "div" l' p' (w s)

-- -- | Convert a 'Display' into a @Many specs -> R.ReactMlT m ()@
-- renderDisplay :: Monad m => Display m specs -> Many specs -> R.ReactMlT m ()
-- renderDisplay (Display (l, p, w)) = fromMaybe mempty $ (\w' -> w' l p) <$> w
