-- | The field of fractions over a GCD domain. The reason that it is an GCD 
-- domain is that we only want to work over reduced quotients.
module Algebra.Structures.FieldOfFractions
  ( FieldOfFractions(..)
  , numerator, denominator
  , toFieldOfFractions, fromFieldOfFractions
  , reduce, propReduce
  ) where

import Test.QuickCheck

import Algebra.Structures.Field
import Algebra.Structures.GCDDomain


-------------------------------------------------------------------------------
-- | Field of fractions

newtype GCDDomain a => FieldOfFractions a = F (a,a)

numerator, denominator :: GCDDomain a => FieldOfFractions a -> a
numerator (F (x,_))   = x
denominator (F (_,x)) = x


--------------------------------------------------------------------------------
-- Instances

instance (GCDDomain a, Show a, Eq a) => Show (FieldOfFractions a) where
  show (F (a,b)) | b == one  = show a
                 | otherwise = case show b of
                    ('-':xs) -> "-" ++ show a ++ "/" ++ xs
                    xs -> show a ++ "/" ++ xs

instance (GCDDomain a, Eq a, Arbitrary a) => Arbitrary (FieldOfFractions a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    if b == zero 
       then return $ F (a,one)
       else return $ reduce $ F (a,b)

instance (GCDDomain a, Eq a) => Eq (FieldOfFractions a) where
  (F (a,b)) == (F (c,d)) = a <*> d == b <*> c
--    where
--    F (a,b) = reduce f
--    F (c,d) = reduce g

instance (GCDDomain a, Eq a) => Ring (FieldOfFractions a) where
  (F (a,b)) <+> (F (c,d)) = reduce (F (a <*> d <+> c <*> b,b <*> d))
  (F (a,b)) <*> (F (c,d)) = reduce (F (a <*> c,b <*> d))
  neg (F (a,b))           = reduce (F (neg a,b))
  one                     = toFieldOfFractions one
  zero                    = toFieldOfFractions zero

instance (GCDDomain a, Eq a) => CommutativeRing (FieldOfFractions a)
instance (GCDDomain a, Eq a) => IntegralDomain (FieldOfFractions a)

instance (GCDDomain a, Eq a) => Field (FieldOfFractions a) where
  inv (F (a,b)) | a /= zero && b /= zero = reduce $ F (b,a)
                | otherwise = error "FieldOfFraction: Division by zero"


--------------------------------------------------------------------------------
-- Operations

-- | Embed a value in the field of fractions.
toFieldOfFractions :: GCDDomain a => a -> FieldOfFractions a
toFieldOfFractions a = F (a,one)

-- | Extract a value from the field of fractions. This is only possible if the
-- divisor is one.
fromFieldOfFractions :: (GCDDomain a, Eq a) => FieldOfFractions a -> a
fromFieldOfFractions x | b' == one = a'
                       | otherwise = error "fromFieldOfFractions: Division by zero"
  where F (a',b') = reduce x 

-- | Reduce an element.
reduce :: (GCDDomain a, Eq a) => FieldOfFractions a -> FieldOfFractions a
reduce (F (a,b)) | b == zero = error "reduce: Division by zero"
                 | a == zero = F (zero,one)
                 | otherwise = F (x,y)
  where
  (g,x,y) = gcd' a b

-- Specification of reduce.
propReduce :: (GCDDomain a, Eq a) => FieldOfFractions a -> Property
propReduce f@(F (a,b)) = a /= zero && b /= zero ==> g == one
  where
  F (c,d) = reduce f
  (g,_,_) = gcd' c d
