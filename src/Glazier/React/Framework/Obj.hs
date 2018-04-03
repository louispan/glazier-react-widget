{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Obj where

import Control.Lens
import GHC.Generics

-- | naming convention:
-- foo this@(Obj var my) = do -- or use RecordWildcards
--     me <- readIORef var
--     writeIORef ref (me & my.bar .~ 5)
--     doSomethingElseWith this
data Obj ref parent a = Obj { ref :: ref parent, my :: Traversal' parent a }

_ref :: Lens' (Obj ref parent a) (ref parent)
_ref = lens ref (\s a -> s { ref = a})

_my :: Lens' (Obj ref parent a) (ReifiedTraversal' parent a)
_my = lens (\(Obj _ a) -> Traversal a) (\s (Traversal a) -> s { my = a})

-- | Tip: This can be used to 'magnify' 'MonadReader' with
-- @magnify ('to' ('access' theLens)) theReader@
-- access :: LensLike' f s a -> Obj ref f p s -> Obj ref f p a
-- access l (Obj t s) = Obj t (s.l)
access :: Traversal' s a -> Obj ref p s -> Obj ref p a
access l (Obj p s) = Obj p (s.l)

-- accessor :: Functor f => LensLike' f s a -> LensLike' f (Obj ref f p s) (Obj ref f p a)
-- accessor l afa (Obj p s) = (\(Obj p' _) -> Obj p' s) <$> afa (Obj p (s.l))
-- accessor :: Traversal' s a -> Lens' (Obj ref p s) (Obj ref p a)
-- accessor l = lens (\(Obj p s) -> Obj p (s.l))
--     (\(Obj _ s) (Obj p _) -> Obj p s)

-- Polymorphic @Lens'@ prevents a auto derivied Generic instance
-- THe custom Generic instance uses 'ReifiedTraversal''
instance Generic (Obj ref p a) where
    from (Obj p a)
        = M1 (M1 (M1 (K1 p) :*: M1 (K1 (Traversal a))))
    to (M1 (M1 (M1 (K1 p) :*: M1 (K1 (Traversal a)))))
        = Obj p a
    type Rep (Obj ref p a) = D1
        ('MetaData
            "Obj"
            "Glazier.React.Framework.Obj"
            "glazier"
            'False)
        (C1
            ('MetaCons
                "Obj"
                'PrefixI
                'True)
            (S1
                ('MetaSel
                    ('Just "ref")
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy)
                (Rec0 (ref p))
                :*: S1
                    ('MetaSel
                        ('Just "my")
                        'NoSourceUnpackedness
                        'NoSourceStrictness
                        'DecidedLazy)
                    (Rec0 (ReifiedTraversal' p a))))

