-- | Representation of Euclidean domains. That is integral domains with an
-- Euclidean functions and decidable division.
--
module Algebra.Structures.EuclideanDomain
  ( EuclideanDomain(..)
  , propNorm, propQuotRem, propEuclideanDomain
  , modulo, quotient, divides
  , euclidAlg, genEuclidAlg
  , lcmE, genLcmE
  , extendedEuclidAlg, genExtendedEuclidAlg
  ) where

import Test.QuickCheck

import Algebra.Structures.IntegralDomain
-- import Algebra.Structures.Coherent
import Algebra.Ideal


-------------------------------------------------------------------------------
-- | Euclidean domains
--
-- Given a and b compute (q,r) such that a = bq + r and r = 0 || norm r < norm b.
-- Where norm is the Euclidean function.

class IntegralDomain a => EuclideanDomain a where
  norm :: a -> Integer
  quotientRemainder :: a -> a -> (a,a)

-- | Check both that |a| <= |ab| and |a| >= 0 for all a,b.
propNorm :: (EuclideanDomain a, Eq a) => a -> a -> Bool
propNorm a b =
  a == zero || b == zero || 
  (norm a <= norm (a <*> b) && norm a >= 0 && norm b >= 0)

propQuotRem :: (EuclideanDomain a, Eq a) => a -> a -> Bool
propQuotRem a b = 
  b == zero || (a == b <*> q <+> r && (r == zero || norm r < norm b))
    where (q,r) = quotientRemainder a b

propEuclideanDomain :: (EuclideanDomain a, Eq a) => a -> a -> a -> Property
propEuclideanDomain a b c =
  if propNorm a b
     then if propQuotRem a b
             then propIntegralDomain a b c
             else whenFail (print "propQuotRem") False
     else whenFail (print "propD") False


-------------------------------------------------------------------------------
-- Operations

modulo :: EuclideanDomain a => a -> a -> a
modulo a b = snd (quotientRemainder a b)

quotient :: EuclideanDomain a => a -> a -> a
quotient a b = fst (quotientRemainder a b)

divides :: (EuclideanDomain a, Eq a) => a -> a -> Bool
divides a b = modulo b a == zero

-- | The Euclidean algorithm for calculating the GCD of a and b.
euclidAlg :: (EuclideanDomain a, Eq a) => a -> a -> a
euclidAlg a b | a == zero && b == zero = zero
              | b == zero = a
              | otherwise = euclidAlg b (a `modulo` b)

-- | Generalized Euclidean algorithm to compute GCD of a list of elements.
genEuclidAlg :: (EuclideanDomain a, Eq a) => [a] -> a
genEuclidAlg = foldl euclidAlg zero

-- | Lowest common multiple, (a*b)/gcd(a,b).
lcmE :: (EuclideanDomain a, Eq a) => a -> a -> a
lcmE a b = quotient (a <*> b) (euclidAlg a b)

-- | Generalized lowest common multiple to compute lcm of a list of elements.
genLcmE :: (EuclideanDomain a, Eq a) => [a] -> a
genLcmE xs = quotient (foldr1 (<*>) xs) (genEuclidAlg xs)

-- | The extended Euclidean algorithm.
--
-- Computes x and y in ax + by = gcd(a,b).
--
extendedEuclidAlg :: (EuclideanDomain a, Eq a) => a -> a -> (a,a)
extendedEuclidAlg a b | a == zero = (zero,one)
                      | b == zero = (one,zero)
--                      | modulo a b == zero = (one,zero)
                      | otherwise          = (y, x <-> y <*> (a `quotient` b))
  where (x,y) = extendedEuclidAlg b (a `modulo` b)

-- Specification of extended Euclidean algorithm.
propExtendedEuclidAlg :: (EuclideanDomain a, Eq a) => a -> a -> Property
propExtendedEuclidAlg a b = a /= zero && b /= zero ==>
  let (x,y) = extendedEuclidAlg a b in a <*> x <+> b <*> y == euclidAlg a b

-- | Generalized extended Euclidean algorithm.
--
-- Solves a_1 x_1 + ... + a_n x_n = gcd (a_1,...,a_n)
--
genExtendedEuclidAlg :: (EuclideanDomain a, Eq a) => [a] -> [a]
genExtendedEuclidAlg [x,y] = let (a,b) = extendedEuclidAlg x y in [a,b]
genExtendedEuclidAlg xs    =
  let (x,y) = extendedEuclidAlg (genEuclidAlg (init xs)) (last xs)
  in map (x<*>) (genExtendedEuclidAlg (init xs)) ++ [y]

-- Specification of generalized extended Euclidean algorithm.
propGenExtEuclidAlg :: (EuclideanDomain a, Eq a) => [a] -> Property
propGenExtEuclidAlg xs = all (/= zero) xs && length xs >= 2 ==>
  foldr (<+>) zero (zipWith (<*>) (genExtendedEuclidAlg xs) xs) == genEuclidAlg xs
