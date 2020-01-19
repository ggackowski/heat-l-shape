-- | Structure for commutative rings. 
--
module Algebra.Structures.CommutativeRing
  ( module Algebra.Structures.Ring
  , CommutativeRing(..)
  , propMulComm, propCommutativeRing
  ) where

import Test.QuickCheck

import Algebra.Structures.Ring


-------------------------------------------------------------------------------
-- | Definition of commutative rings.

class Ring a => CommutativeRing a

propMulComm :: (CommutativeRing a, Eq a) => a -> a -> Bool
propMulComm a b = a <*> b == b <*> a


-- | Specification of commutative rings. Test that multiplication is 
-- commutative and that it satisfies the ring axioms.
propCommutativeRing :: (CommutativeRing a, Eq a) => a -> a -> a -> Property
propCommutativeRing a b c = if propMulComm a b 
                               then propRing a b c 
                               else whenFail (print "propMulComm") False
