-- | Specification of principal localization matrices used in the coherence 
-- proof of Prufer domains. 
module Algebra.PLM 
  ( propPLM
  , computePLM_B
  ) where

import Algebra.Structures.CommutativeRing
import Algebra.Structures.BezoutDomain
import Algebra.Ideal
import Algebra.Matrix


-------------------------------------------------------------------------------
{- | A principal localization matrix for an ideal (x1,...,xn) is a matrix such 
that:

 - The sum of the diagonal should equal 1.

 - For all i, j, l in {1..n}: a_lj * x_i = a_li * x_j

-}
propPLM :: (CommutativeRing a, Eq a) => Ideal a -> Matrix a -> Bool
propPLM (Id xs) (M vs) 
  | isSquareMatrix (M vs) = 
    let as      = map unVec vs
        sumDiag = sumRing [ ai !! i | (ai,i) <- zip as [0..]]
        n       = length as - 1
        ijl     = and [ as !! l !! j <*> xs !! i == 
                        as !! l !! i <*> xs !! j
                      | i <- [0..n]
                      , j <- [0..n]
                      , l <- [0..n]
                      ]
    in one == sumDiag && ijl
  | otherwise = error "isPLM: Not square matrix"

-- Maybe it would be nice to add some of the properties in Proposition 2.6 in:
-- http://hlombardi.free.fr/liens/salouThesis.pdf


-------------------------------------------------------------------------------
-- | Principal localization matrices for ideals are computable in Bezout domains.

computePLM_B :: (BezoutDomain a, Eq a) => Ideal a -> Matrix a
computePLM_B (Id xs) = 
  let (Id [g],us,ys) = toPrincipal (Id xs)
      n              = length xs - 1
  in M [ Vec [ us !! i <*> ys !! j | j <- [0..n] ] | i <- [0..n] ]
