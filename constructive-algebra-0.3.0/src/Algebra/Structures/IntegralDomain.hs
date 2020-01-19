module Algebra.Structures.IntegralDomain
  ( module Algebra.Structures.CommutativeRing
  , IntegralDomain
  , propZeroDivisors, propIntegralDomain
  ) where

import Test.QuickCheck

import Algebra.Structures.Ring
import Algebra.Structures.CommutativeRing


-------------------------------------------------------------------------------
-- | Definition of integral domains.

class CommutativeRing a => IntegralDomain a

-- An integral domain is a ring in which there are no zero divisors.
propZeroDivisors :: (IntegralDomain a, Eq a) => a -> a -> Bool
propZeroDivisors a b = if a <*> b == zero then a == zero || b == zero else True


-- | Specification of integral domains. Test that there are no zero-divisors
-- and that it satisfies the axioms of commutative rings.
propIntegralDomain :: (IntegralDomain a, Eq a) => a -> a -> a -> Property
propIntegralDomain a b c = if propZeroDivisors a b
                              then propCommutativeRing a b c 
                              else whenFail (print "propZeroDivisors") False
