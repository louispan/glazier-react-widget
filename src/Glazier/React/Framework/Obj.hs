{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Obj where

import Control.Lens
import Data.IORef
import GHC.Generics

data Obj v s = Obj
    { ref :: IORef v
    , its :: Lens' v s
    }

edit :: Lens' s a -> Obj v s -> Obj v a
edit l (Obj v i) = Obj v (i.l)

instance Generic (Obj v s) where
    from (Obj v s)
        = M1 (M1 (M1 (K1 v) :*: M1 (K1 (Lens s))))
    to (M1 (M1 (M1 (K1 v) :*: M1 (K1 (Lens s)))))
        = Obj v s
    type Rep (Obj v s) = D1
        ('MetaData
            "Obj"
            "Glazier.React.Framework.Obj"
            "glazier-react-widget"
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
                (Rec0 (IORef v))
                :*: S1
                    ('MetaSel
                        ('Just "its")
                        'NoSourceUnpackedness
                        'NoSourceStrictness
                        'DecidedLazy)
                    (Rec0 (ReifiedLens' v s))))

