{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.IsReader where

class IsReader r n | n -> r where
    type ReaderResult r n
    fromReader :: (r -> ReaderResult r n) -> n
    toReader :: n -> (r -> ReaderResult r n)

instance IsReader s (s -> a) where
    type ReaderResult s (s -> a) = a
    fromReader = id
    toReader = id
