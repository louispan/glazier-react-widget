{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Widgets.Collection where

import Control.Lens
import Glazier.React

collectionWindow ::
    ( Foldable t
    , Monoid (t s')
    )
    => Traversal' s (t s')
    -> (s' -> Window s ())
    -> Window s ()
collectionWindow this f = do
    ss <- view (_meta.this)
    getAls (foldMap (Als . f) ss)

-- mkCollection :: (s'' -> m s') -> t s'' -> m (Obj s)
-- mkCollection =

-- collectionWindow :: (Functor t, Foldable t)
--     => ReactId -> Window (t (Obj s)) ()
-- collectionWindow k = do
--     ss <- view _meta
--     let displayItem s = Als $ (displayWeakObj $ weakObj s)
--     bh "ul" [("key", JE.toJSRep . J.pack $ show k)] (getAls (fold $ displayItem <$> ss))

-- foldableWindow ::
--     ( Foldable t
--     , Monoid (t (Obj s'))
--     )
--     => (Window s () -> Window s ())
--     -> Traversal' s (t (Obj s'))
--     -> Window (t (Obj s') ()
-- foldableWindow f this = do
--     ss <- view (_meta.this)
--     getAls (foldMap (Als . f . displayWeakObj . weakObj) ss)

-- -- FIXME: what about zoomin sate into list?
-- pureCollection ::
--     ( Foldable t
--     , MonadWidget c s m
--     )
--     => t (m ()) -> m ()
-- pureCollection ms = do
--     bh "ul" [] []
--         -- for each of the widget, wrap it in a li
--         (getAls (foldMap (Als . bh "li" [] []) ms))

