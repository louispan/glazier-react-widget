{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.IsReader where

class IsReader r a | a -> r where
    type ReaderResult r a
    fromReader :: (r -> ReaderResult r a) -> a
    toReader :: a -> (r -> ReaderResult r a)
