{-# LANGUAGE MultiParamTypeClasses,  ScopedTypeVariables, TypeOperators,
             FunctionalDependencies, FlexibleContexts,    UndecidableInstances,
             FlexibleInstances #-}
-- | Integers modulo n parametrised by the n. This also has type-level primality
-- testing used for instantiating integral domain and field type classes. The
-- primality testing is very slow, but it seem to be working fine for relatively
-- small numbers.
module Algebra.Zn (Zn(..), Z3) where

import Data.TypeLevel hiding ((+),(-),(*),mod,Eq,(==))
import Control.Monad (liftM)
import Test.QuickCheck

import Algebra.Structures.Field
import Algebra.Z

-- | The phantom type n represents which modulo to work in.
newtype Zn n = Zn Integer
  deriving (Eq,Ord)

instance Show (Zn n) where
  show (Zn n) = show n

instance Nat n => Num (Zn n) where
  Zn x + Zn y   = Zn $ (x+y) `mod` toNum (undefined :: n)
  Zn x * Zn y   = Zn $ (x*y) `mod` toNum (undefined :: n)
  abs (Zn x)    = Zn $ abs x
  signum (Zn x) = Zn $ signum x
  negate (Zn x) = Zn $ negate x `mod` toNum (undefined :: n)
  fromInteger x = Zn $ fromInteger $ x `mod` toNum (undefined :: n)

instance Nat n => Arbitrary (Zn n) where
  arbitrary = liftM Zn (choose (0,toNum (undefined :: n) - 1))

instance Nat n => Ring (Zn n) where
  (<+>) = (+)
  zero  = Zn 0
  one   = Zn 1
  neg   = negate
  (<*>) = (*)

instance Nat n => CommutativeRing (Zn n) where

instance (Prime n True, Nat n) => IntegralDomain (Zn n) where

instance (Prime n True, Nat n) => Field (Zn n) where
  inv (Zn x) | x == 1         = Zn 1
             | p `mod` x == 0 = error "Can't find the inverse of zero!"
             | otherwise      = Zn $ x <^> (p-2) `mod` p
    where p = toNum (undefined :: n)

-- Z6 is not an integral domain and the typechecker will spot it!
-- intDomZ6 = quickCheck (propIntegralDomain :: Z6 -> Z6 -> Z6 -> Property)

-- Tests:

type Z3 = Zn D3

test1 :: Z3
test1 = inv 2

type Z17 = Zn D17

-- test2 :: Z17
-- test2 = inv 13

-- Test that all elements of Z17 get correct inverses
-- test3 :: Prelude.Bool
-- test3 = all (==1) [ inv x * x | x <- xs ]
--   where xs :: [Z17] = map fromInteger [1..16]

-----------------------------------------------------------------------
-- Lots of crazy type-level stuff:

class (Nat x, Nat sqrt) => Sqrt x sqrt | x -> sqrt
instance (Nat x, Nat sqrt, Sqrt' x D1 GT sqrt) => Sqrt x sqrt

class Sqrt' x y r sqrt | x y r -> sqrt
instance Sub y D2 y' => Sqrt' x y LT y'
instance Pred y y'   => Sqrt' x y EQ y'
instance (ExpBase y D2 square, Succ y y', Trich x square r,
          Sqrt' x y' r sqrt) => Sqrt' x y GT sqrt

sqrt :: Sqrt x sqrt => x -> sqrt
sqrt = undefined

class (Nat x, Data.TypeLevel.Bool b) => Prime x b | x -> b
instance (Sqrt x y, Trich y D1 r, Prime' x y r b) => Prime x b

class Data.TypeLevel.Bool b => Prime' x y r b | x y r -> b
instance Prime' x D1 EQ True
instance (Pred y z, Trich z D1 r1, Mod x y rest, IsZero rest b1,
          Not b1 b', Prime' x z r1 b2, And b' b2 b3) => Prime' x y GT b3

prime :: Prime x b => x -> b
prime = undefined

class IsZero x r | x -> r
instance IsZero D0 True
instance IsZero D1 False
instance IsZero D2 False
instance IsZero D3 False
instance IsZero D4 False
instance IsZero D5 False
instance IsZero D6 False
instance IsZero D7 False
instance IsZero D8 False
instance IsZero D9 False
instance Pos x => IsZero (x :* d) False
