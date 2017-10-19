-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE KindSignatures #-}

module Glazier.React.Framework.Executor where

-- import Control.Applicative
-- import Control.Monad.Trans.Maybe
-- import Data.Functor.Contravariant
-- import Data.Diverse
-- import Data.Tagged
-- import Glazier.React.Framework.Widget as F

-- newtype Executor c = Executor
--     { runExecutor :: c -> MaybeT IO ()
--     }

-- instance Contravariant Executor where
--     contramap f (Executor exec) = Executor $ exec . f

-- noop :: Executor (F.Whichever '[] c)
-- noop = Executor . const $ pure ()

-- -- | 'noop' is also identity for 'orExecutor'
-- -- | The intention of this combinator is to allow combining executors for different
-- -- input actions together.
-- -- Therefore, it is will be compile error to `orExecutor` of the same input types.
-- -- This is to prevent accidently processing an action twice.
-- -- The compile error only appears at the point when 'F.runWhichever'
-- -- is used to extract the Which type.
-- -- The compile error will be due to @(Append c1 c2)@ which will not satisfy
-- -- @UniqueMember@ constraints.
-- -- NB. The use of <|> only the first executor for a particular action will be used.
-- orExecutor
--     :: Executor (F.Whichever c1 c)
--     -> Executor (F.Whichever c2 c)
--     -> Executor (F.Whichever (Append c1 c2) c)
-- orExecutor (Executor r) (Executor r') =
--     Executor (\(Tagged c) -> r (Tagged c) <|> r' (Tagged c))

-- executor'
--     :: (UniqueMember c' c)
--     => (c' -> MaybeT IO ()) -> Executor (F.Whichever '[c'] c)
-- executor' f = Executor $ \(Tagged c) -> case trial' c of
--             Nothing -> empty
--             Just c' -> f c'

-- executor
--     :: Reinterpret' c' c
--     => (Which c' -> MaybeT IO ()) -> Executor (F.Whichever c' c)
-- executor f = Executor $ \(Tagged c) -> case reinterpret' c of
--         Nothing -> empty
--         Just c' -> f c'
