{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Executor where

import Data.Diverse
import qualified Data.DList as DL

type Executor c = c -> IO ()

-- | 'nulExecutor' is also identity for 'orExecutor'
nulExecutor :: Executor (Which '[])
nulExecutor = const $ pure ()

-- | 'nulListExecutor' is also identity for 'orExecutor'
nulListExecutor :: Executor (DL.DList (Which '[]))
nulListExecutor = const $ pure ()

-- | Ignore certain inputs
lfilterExecutor :: (a' -> Maybe a) -> Executor a -> Executor a'
lfilterExecutor f exec a' = case f a' of
    Nothing -> pure ()
    Just a -> exec a

-- | The intention of this combinator is to allow combining executors for different
-- input actions together.
-- Therefore, it is will be compile error to `orExecutor` of the with the same input types.
-- This is to prevent accidently processing an action twice.
-- The compile error will be due to @(Append c1 c2)@ which will not satisfy
-- @UniqueMember@ constraints.
-- NB. The use of <|> only the first executor for a particular action will be used.
orExecutor
    :: (Reinterpret c2 (Append c1 c2), (Complement (Append c1 c2) c2) ~ c1)
    => Executor (Which c1)
    -> Executor (Which c2)
    -> Executor (Which (Append c1 c2))
orExecutor x y c =
    case reinterpret c of
        Left c' -> x c'
        Right c' -> y c'

-- | Variation of 'orExecutor' for combing 'DL.DList' of 'Which'es.
orListExecutor
    :: ( Reinterpret' c2 (Append c1 c2)
       , Reinterpret' c1 (Append c1 c2)
       , (Complement (Append c1 c2) c2) ~ c1
       , (Complement (Append c1 c2) c1) ~ c2
       )
    => Executor (DL.DList (Which c1))
    -> Executor (DL.DList (Which c2))
    -> Executor (DL.DList (Which (Append c1 c2)))
orListExecutor x y cs = let lcs = foldMap lgo cs
                            rcs = foldMap rgo cs
                        in x lcs >> y rcs
  where
    lgo c = case reinterpret' c of
        Nothing -> DL.empty
        Just c' -> DL.singleton c'
    rgo c = case reinterpret' c of
        Nothing -> DL.empty
        Just c' -> DL.singleton c'
