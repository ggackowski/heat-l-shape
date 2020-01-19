module Algebra.Structures.StronglyDiscrete 
  ( StronglyDiscrete(member)
  , propStronglyDiscrete
  ) where

import Algebra.Structures.CommutativeRing
import Algebra.Ideal


-------------------------------------------------------------------------------
-- | Strongly discrete rings
--
-- A ring is called strongly discrete if ideal membership is decidable.
-- Nothing correspond to that x is not in the ideal and Just is the witness.
-- Examples include all Bezout domains and polynomial rings.
--
class Ring a => StronglyDiscrete a where
  member :: a -> Ideal a -> Maybe [a]

-- | Test that the witness is actually a witness that the element is in the 
-- ideal.
propStronglyDiscrete :: (CommutativeRing a, StronglyDiscrete a, Eq a)
                     => a -> Ideal a -> Bool                  
propStronglyDiscrete x id@(Id xs) = case member x id of
  Just as -> x == sumRing (zipWith (<*>) xs as) && length xs == length as
  Nothing -> True
