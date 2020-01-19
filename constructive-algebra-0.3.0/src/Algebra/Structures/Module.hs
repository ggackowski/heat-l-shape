{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
-- | R-modules. 
module Algebra.Structures.Module 
  ( Module((*>)), (<*)
  , propScalarMul, propScalarAdd, propScalarAssoc, propModule
  ) where

import Algebra.Structures.Group as G
import Algebra.Structures.CommutativeRing as R
import Algebra.Z
import Algebra.Zn

import Test.QuickCheck

infixl 7 *>
infixr 7 <*

-- Consider only the commutative case, it would be possible to implement left
-- and right modules instead.

-- | Module over a commutative ring r.
class (CommutativeRing r, AbelianGroup m) => Module r m where
  -- | Scalar multiplication.
  (*>) :: r -> m -> m

propScalarMul :: (Module r m, Eq m) => r -> m -> m -> Bool
propScalarMul r x y = r *> (x G.<+> y) == (r *> x) G.<+> (r *> y)

propScalarAdd :: (Module r m, Eq m) => r -> r -> m -> Bool
propScalarAdd r s x = (r R.<+> s) *> x == (r *> x) G.<+> (s *> x)

propScalarAssoc :: (Module r m, Eq m) => r -> r -> m -> Bool
propScalarAssoc r s x = (r <*> s) *> x == r *> (s *> x)

propModule :: (Module r m, Eq m) => r -> r -> m -> m -> Property
propModule r s x y =
  case (propScalarMul r x y, propScalarAdd r s x, propScalarAssoc r s x) of
    (True,True,True) -> whenFail (return ()) True
    (False,_,_)      -> whenFail (print "propScalarMul") False
    (_,False,_)      -> whenFail (print "propScalarAdd") False
    (_,_,False)      -> whenFail (print "propScalarAssoc") False

-- | Since the ring is commutative we can turn the scalar multiplication around.
(<*) :: Module r m => m -> r -> m
(<*) = flip (*>)

-- | Z-module structure.
instance AbelianGroup m => Module Z m where
  n *> x | n > 0  = sumGroup (replicate (fromInteger n) x)
         | n == 0 = G.zero
         | n < 0  = G.neg (abs n *> x)


-- Sinze Z3 is an abelian group we get that it is a Z module for free:

test1 :: Z3
test1 = (2 :: Z) *> 5

test2 = quickCheck (propModule :: Z -> Z -> Z3 -> Z3 -> Property)

-- Vector spaces:
-- 
-- There should be some nice way to do type-class aliases, something like:
--
-- type VectorSpace k m = Field k => Module k m
