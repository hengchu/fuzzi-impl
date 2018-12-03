module Algebra where

import Data.Comp.Algebra

-- product f-algebra given two algebras
prod :: (Functor f) => Alg f a -> Alg f b -> Alg f (a, b)
prod alg1 alg2 v = (alg1 $ fmap fst v, alg2 $ fmap snd v)

prodM :: (Functor f, Monad m) => AlgM m f a -> AlgM m f b -> AlgM m f (a, b)
prodM alg1 alg2 v = do
  v1 <- alg1 $ fmap fst v
  v2 <- alg2 $ fmap snd v
  return (v1, v2)

-- Build a complex monadic algebra using:
-- 1. a compose   function 'f'
-- 2. a decompose function 'g'
-- 3. an algebra over for the decomposed structure

-- The compose and decompose functions may use monadic effects to rule out
-- certain combinations.

-- Use case:
-- This allows us to build "speculative" checkers in parallel, and combine these
-- "speculative" checkers together, and use the smart compose function to filter
-- out unsound results.
buildAlgM :: (Monad m, Traversable f)
          => (a -> m b) -> (b -> m a) -> AlgM m f a -> AlgM m f b
buildAlgM f g alg fb = mapM g fb >>= alg >>= f
