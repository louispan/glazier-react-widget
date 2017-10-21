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
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F

newtype Display m s = Display
    { runDisplay :: s -> R.ReactMlT m ()
    } deriving (Semigroup)

instance Contravariant (Display m) where
    contramap f (Display disp) = Display $ disp . f

instance Monad m => Monoid (Display m s) where
    mempty = Display mempty
    mappend = (<>)

instance F.Modeller (Display m s) (Display m) s where
    toModeller = id
    fromModeller = id

instance R.MonadReactor m => F.ViaModel (Display m) where
    viaModel l = contramap (view l)

instance R.MonadReactor m => F.IORefModel (Display m s) (Display m (IORef s)) where
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
