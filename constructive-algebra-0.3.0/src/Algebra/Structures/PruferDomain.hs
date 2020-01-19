{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- | Prufer domains are non-Noetherian analogues of Dedekind domains. That is
-- integral domains in which every finitely generated ideal is invertible. This 
-- implementation is mainly based on:
--
-- http:\/\/hlombardi.free.fr\/liens\/salouThesis.pdf
--
module Algebra.Structures.PruferDomain 
  ( PruferDomain(..)
  , propCalcUVW, propPruferDomain
  , calcUVW_B, calcUVWT, propCalcUVWT, fromUVWTtoUVW
  , computePLM_PD
  , invertIdeal
  , intersectionPD, intersectionPDWitness, solvePD
  ) where

import Test.QuickCheck
import Data.List (nub, (\\))

import Algebra.Structures.IntegralDomain
import Algebra.Structures.BezoutDomain
import Algebra.Structures.Coherent
import Algebra.Ideal
import Algebra.Matrix


-------------------------------------------------------------------------------
-- | Given a and b it computes u, v and we such that:
-- 
--  (1) au = bv
--
--  (2) b(1-u) = aw
--
class IntegralDomain a => PruferDomain a where
  --         a    b     u v w
  calcUVW :: a -> a -> (a,a,a)

-- | Property specifying that:
-- au = bv and b(1-u) = aw
propCalcUVW :: (PruferDomain a, Eq a) => a -> a -> Bool
propCalcUVW a b = a <*> u == b <*> v && b <*> (one <-> u) == a <*> w
  where (u,v,w) = calcUVW a b

propPruferDomain :: (PruferDomain a, Eq a) => a -> a -> a -> Property
propPruferDomain a b c | propCalcUVW a b = propIntegralDomain a b c
                       | otherwise       = whenFail (print "propCalcUVW") False


-- | Alternative characterization of Prufer domains, given a and b compute u, v, 
-- w, t such that:
-- 
-- ua = vb && wa  = tb && u+t = 1
calcUVWT :: PruferDomain a => a -> a -> (a,a,a,a)
calcUVWT a b = (x,y,z,one <-> x)
  where (x,y,z) = calcUVW a b

propCalcUVWT :: (PruferDomain a, Eq a) => a -> a -> Bool
propCalcUVWT a b = u <*> a == v <*> b && w <*> a == t <*> b && u <+> t == one
  where (u,v,w,t) = calcUVWT a b

-- | Go back to the original definition.
fromUVWTtoUVW :: (a,a,a,a) -> (a,a,a)
fromUVWTtoUVW (u,v,w,t) = (u,v,w) 


--------------------------------------------------------------------------------
-- | Bezout domain -> Prufer domain
--
{-
Prufer: forall a b exists u v w t.  u+t = 1 &  ua = vb & wa = tb

We consider only domain.
We assume we have the Bezout condition: given a, b we can find g,a1,b1,c,d s.t.

a = g a1
b = g b1
1 = c a1 + d b1

We try then 

u = d b1
t = c a1

We should find v such that
a d b1 = b v
this simplifies to 
g a1 d b1 = g b1 v
and we can take 
v = a1 d
Similarly we can take 
w = b1 c

We have shown that Bezout domain -> Prufer domain.

instance (BezoutDomain a, Eq a) => PruferDomain a where
-}

-- | Proof that all Bezout domains are Prufer domains.
calcUVW_B :: (BezoutDomain a, Eq a) => a -> a -> (a,a,a)
calcUVW_B a b | a == zero = (one,zero,zero)
              | b == zero = (zero,zero,zero)
              | otherwise = fromUVWTtoUVW (u,v,w,t)
    where
    -- Compute g, a1 and b1 such that:
    -- a = g*a1
    -- b = g*b1
    (g,[_,_],[a1,b1])  = toPrincipal (Id [a,b])
    
    -- Compute c and d such that:
    -- 1 = a1*c + a2*d
    (_,[c,d],_) = toPrincipal (Id [a1,b1])

    u = d <*> b1
    t = c <*> a1
    v = d <*> a1
    w = c <*> b1

-------------------------------------------------------------------------------
-- Coherence

-- | Compute a principal localization matrix for an ideal in a Prufer domain.
computePLM_PD :: (PruferDomain a, Eq a) => Ideal a -> Matrix a
computePLM_PD (Id [_])   = matrix [[one]]
computePLM_PD (Id [a,b]) = let (u,v,w,t) = calcUVWT b a 
                           in M [ Vec [u,v], Vec [w,t]]
computePLM_PD (Id xs)    = matrix a
  where
  -- Use induction hypothesis to construct a matrix for n-1:
  x_is = init xs
  b    = unMVec $ computePLM_PD (Id x_is)
  m    = length b - 1
  
  -- Let s_i be b_ii:
  s_is = [ (b !! i) !! i | i <- [0..m]]

  -- Take out x_n:
  x_n  = last xs

  -- Compute (u_i, v_i, w_i, t_i) for <x_n,x_i>:
  uvwt_i = [ calcUVWT x_n x_i | x_i <- x_is ]
    
  -- Take out all u, v, w, and t:
  u_is = [ u_i | (u_i,_,_,_) <- uvwt_i ]
  v_is = [ v_i | (_,v_i,_,_) <- uvwt_i ]
  w_js = [ w_i | (_,_,w_i,_) <- uvwt_i ]
  t_is = [ t_i | (_,_,_,t_i) <- uvwt_i ]
  
  -- COMPUTE a_ij for 1 <= i,j < n
  -- i = row
  -- j = column
  a_ij = [ [ if i == j 
                then (s_is !! i) <*> (u_is !! i)
                else (u_is !! i) <*> (b !! i !! j)
           | j <- [0..m] ]
         | i <- [0..m] ]

  -- COMPUTE a_nn
  a_nn = sumRing $ zipWith (<*>) s_is t_is

  -- COMPUTE a_ni for 1 <= i < n
  -- THIS IS THE LAST ROW
  a_ni = [ sumRing [ (b !! j !! i) <*> (w_js !! j)
                   | j <- [0..m] ]
         | i <- [0..m] ]

  -- COMPUTE a_in for 1 <= i < n
  -- THIS IS THE LAST COLUMN
  a_in = [ (s_is !! i) <*> (v_is !! i)
         | i <- [0..m] ]

  -- ASSEMBLE EVERYTHING
  a = [ x ++ [y] | (x,y) <- zip a_ij a_in ] ++ [a_ni ++ [a_nn]]


-- | Ideal inversion. Given I compute J such that IJ is principal.
-- Uses the principal localization matrix for the ideal.
invertIdeal :: (PruferDomain a, Eq a) => Ideal a -> Ideal a
invertIdeal xs = 
  let a = unMVec $ computePLM_PD xs

      -- Pick out the first column
      a_njs = [ head (a !! j) | j <- [0..length a - 1]]
  in Id a_njs

-- | Compute the intersection of I and J by:
--       
--       (I \\cap J)(I + J) = IJ  => (I \\cap J)(I + J)(I + J)' = IJ(I + J)'
--
intersectionPDWitness :: (PruferDomain a, Eq a) => Ideal a -> Ideal a -> (Ideal a,[[a]],[[a]])
intersectionPDWitness (Id is) (Id js) = (int,wis,wjs)
  where
  lj  = length js 
  li  = length is

  ij  = Id (is ++ js)

  plm = computePLM_PD ij

  as  = take li $ unMVec $ transpose plm
  as' = drop li $ unMVec $ transpose plm

  int = Id $ concat [ map (j <*>) a | j <- js , a <- as ]

  wis = concat [ [ addZ i li a | a <- as ] | as <- as', i <- [0..li-1] ]
  wjs = [ addZ i lj a | i <- [0..lj-1], a <- concat as ]

  addZ n l x = replicate n zero ++ x : replicate (l-n-1) zero

intersectionPD :: (PruferDomain a, Eq a) => Ideal a -> Ideal a -> Ideal a
intersectionPD i j = fst3 (intersectionPDWitness i j)
  where fst3 (x,_,_) = x

-- | Coherence of Prufer domains.
solvePD :: (PruferDomain a, Eq a) => Vector a -> Matrix a
solvePD x = solveWithIntersection x intersectionPDWitness

-- instance (PruferDomain a, Eq a) => Coherent a where
--   solve = solvePD
