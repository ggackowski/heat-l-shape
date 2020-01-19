-- | Finitely generated ideals in commutative rings.
module Algebra.Ideal
  ( Ideal(Id)
  , zeroIdeal, isPrincipal, fromId
  , eval, addId, mulId
  , isSameIdeal, zeroIdealWitnesses
  ) where

import Data.List (intersperse,nub)
import Test.QuickCheck

import Algebra.Structures.CommutativeRing

-------------------------------------------------------------------------------
-- | Ideals characterized by their list of generators.

data CommutativeRing a => Ideal a = Id [a]

instance (CommutativeRing a, Show a) => Show (Ideal a) where
  show (Id xs) = "<" ++ concat (intersperse "," (map show xs)) ++ ">"

instance (CommutativeRing a, Arbitrary a, Eq a) => Arbitrary (Ideal a) where
  arbitrary = do xs' <- arbitrary
                 let xs = filter (/= zero) xs'
                 if xs == [] then return (Id [one]) else return (Id (nub xs))

-- | The zero ideal.
zeroIdeal :: CommutativeRing a => Ideal a
zeroIdeal = Id [zero]

-- | Test if an ideal is principal.
isPrincipal :: CommutativeRing a => Ideal a -> Bool
isPrincipal (Id xs) = length xs == 1

fromId :: CommutativeRing a => Ideal a -> [a]
fromId (Id xs) = xs

-- | Evaluate the ideal at a certain point.
eval :: CommutativeRing a => a -> Ideal a -> a
eval x (Id xs) = foldr (<+>) zero (map (<*> x) xs)

-- | Addition of ideals.
addId :: (CommutativeRing a, Eq a) => Ideal a -> Ideal a -> Ideal a
addId (Id xs) (Id ys) = Id (nub (xs ++ ys))

-- |  Multiplication of ideals.
mulId :: (CommutativeRing a, Eq a) => Ideal a -> Ideal a -> Ideal a
mulId (Id xs) (Id ys) = if zs == [] then zeroIdeal else Id zs
  where zs = nub [ f <*> g | f <- xs, g <- ys, f <*> g /= zero ]

{- | Test if an operations compute the correct ideal. 
The operation should give a witness that the comuted ideal contains
the same elements.

If \[ x_1, ..., x_n \] \`op\` \[ y_1, ..., y_m \] = \[ z_1, ..., z_l \]

Then the witness should give that

z_k = a_k1 * x_1 + ... + a_kn * x_n
    = b_k1 * y_1 + ... + b_km * y_m

This is used to check that the intersection computed is correct.

-}
isSameIdeal :: (CommutativeRing a, Eq a) 
            => (Ideal a -> Ideal a -> (Ideal a, [[a]], [[a]]))
            -> Ideal a 
            -> Ideal a 
            -> Bool
isSameIdeal op (Id xs) (Id ys) = 
  let (Id zs, as, bs) = (Id xs) `op` (Id ys)
  in length as == length zs && length bs == length zs
     &&
     and [ z_k == sumRing (zipWith (<*>) a_k xs) && length a_k == length xs
         | (z_k,a_k) <- zip zs as ]
     &&
     and [ z_k == sumRing (zipWith (<*>) b_k ys) && length b_k == length ys
         | (z_k,b_k) <- zip zs bs ]


-- | Compute witnesses for two lists for the zero ideal. This is used when 
-- computing the intersection of two ideals.
zeroIdealWitnesses :: (CommutativeRing a) => [a] -> [a] -> (Ideal a, [[a]], [[a]])
zeroIdealWitnesses xs ys = ( zeroIdeal
                           , [replicate (length xs) zero]
                           , [replicate (length ys) zero])
