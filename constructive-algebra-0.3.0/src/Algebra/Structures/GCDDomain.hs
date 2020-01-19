-- | Greatest common divisor (GCD) domains. 
--
-- GCD domains are integral domains in which every pair of nonzero elements 
-- have a greatest common divisor. They can also be characterized as 
-- non-Noetherian analogues of unique factorization domains.
--
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Algebra.Structures.GCDDomain 
  ( GCDDomain(gcd')
  , propGCD, propGCDDomain
  , ggcd
  ) where

import Test.QuickCheck

import Algebra.Structures.IntegralDomain
import Algebra.Structures.BezoutDomain
import Algebra.Ideal

-- infix 4 ~~

-------------------------------------------------------------------------------
-- | GCD domains

class IntegralDomain a => GCDDomain a where
  -- | Compute gcd(a,b) = (g,x,y) such that g = gcd(a,b) and
  --   a = gx
  --   b = gy
  -- and a, b /= 0
  gcd' :: a -> a -> (a,a,a)


propGCD :: (GCDDomain a, Eq a) => a -> a -> Bool
propGCD a b = a == zero || b == zero || a == g <*> x && b == g <*> y
  where
  (g,x,y) = gcd' a b


-- | Specification of GCD domains. They are integral domains in which every 
-- pair of nonzero elements have a greatest common divisor.
propGCDDomain :: (Eq a, GCDDomain a, Arbitrary a, Show a) => a -> a -> a -> Property
propGCDDomain a b c = if propGCD a b 
                         then propIntegralDomain a b c
                         else whenFail (print "propGCD") False

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

-- Generalized greatest common divisor, computes the gcd of a list of elements.
ggcd :: GCDDomain a => [a] -> a
ggcd []     = error "ggcd: Can't compute ggcd of the empty list"
ggcd (x:xs) = foldr (\a b -> fst3 (gcd' a b)) x xs

instance BezoutDomain a => GCDDomain a where
  gcd' a b = (g,x,y)
   where (Id [g],_,[x,y]) = toPrincipal (Id [a,b])

{-
class IntegralDomain a => DecidableUnits a where
  unit :: a -> Maybe a -- Just x = the inverse

-- Divisibility is decidable if A is a gcd domain with decidable units
divides :: (GCDDomain a, DecidableUnits a) => a -> a -> Bool
divides a b = unit g
  where (g,_,_) = gcd' a b

(~~) :: (GCDDomain a, DecidableUnits a) => a -> a -> Bool
(~~) = divides
-}
