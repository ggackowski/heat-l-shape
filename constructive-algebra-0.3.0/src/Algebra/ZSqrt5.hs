{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Proof that Z[sqrt(-5)] is a Prufer domain. This implies that it is 
-- possible to solve systems of equations over Z[sqrt(-5)].
module Algebra.ZSqrt5 (ZSqrt5(..)) where

import Test.QuickCheck


import Algebra.Structures.IntegralDomain -- hiding ((*>),(<*))
import Algebra.Structures.Module
import Algebra.Structures.EuclideanDomain (quotient, genEuclidAlg)
import Algebra.Structures.BezoutDomain (toPrincipal)
import Algebra.Structures.PruferDomain
import Algebra.Structures.Coherent
import Algebra.Ideal
import Algebra.Z
import Algebra.Q

-- | Z[sqrt(-5)] is a pair such that (a,b) = a + b*sqrt(-5)
newtype ZSqrt5 = ZSqrt5 (Z,Z)
  deriving (Eq,Ord,Arbitrary)

instance Show ZSqrt5 where
  show (ZSqrt5 (a,b)) = show a ++ " + " ++ show b ++ " * sqrt(-5)"

-- Arithmetical properties
instance Ring ZSqrt5 where
  (ZSqrt5 (a,b)) <+> (ZSqrt5 (c,d)) = ZSqrt5 (a + c, b + d)
  (ZSqrt5 (a,b)) <*> (ZSqrt5 (c,d)) = ZSqrt5 (a*c - 5*b*d, a*d + b*c)
  neg (ZSqrt5 (a,b))                = ZSqrt5 (neg a, neg b)
  zero                              = ZSqrt5 (0,0)
  one                               = ZSqrt5 (1,0)
  
instance CommutativeRing ZSqrt5 where

instance IntegralDomain ZSqrt5 where

propIntDomZSqrt5 :: ZSqrt5 -> ZSqrt5 -> ZSqrt5 -> Property
propIntDomZSqrt5 = propIntegralDomain

--------------------------------------------------------------------------------
-- Useful auxiliary functions:


(+>) :: Z -> ZSqrt5 -> ZSqrt5
r +> (ZSqrt5 (a,b)) = ZSqrt5 (r+a,b)

(<+) :: ZSqrt5 -> Z -> ZSqrt5
(ZSqrt5 (a,b)) <+ r = ZSqrt5 (a+r,b)

infixl 6 +>, <+

--------------------------------------------------------------------------------

instance PruferDomain ZSqrt5 where
  -- Assume /= 0
  calcUVW (ZSqrt5 (a,b)) (ZSqrt5 (c,d)) = (u,v,w) 
    where
    -- Let s = (a+b*sqrt(-5))/(c+d*sqrt(-5))
    -- Compute p and q such that: s = p + q*sqrt(-5)
    p = toQ (a*c+5*b*d) / n
    q = toQ (b*c-a*d)   / n
    n = toQ (c^2+5*d^2)

    s :: (Q,Q)
    s = (p,q)

    -- Rewrite: 
    -- s = p + q*sqrt(-5)  <==>  s^2 - 2*sp + p^2 + 5*q^2 = 0
    --                     <==> a0*s^2 + a1*s + a2 = 0
    --
    -- Some computations give:
    a0' = toZ n
    a1' = -2 * toZ (p*n)
    a2' = a^2 + 5*b^2 
    
    -- Normalize:
    g  = genEuclidAlg [a0',a1',a2']
    a0 = a0' `quotient` g
    a1 = a1' `quotient` g
    a2 = a2' `quotient` g

    -- Compute m0, m1, m2, m3 such that:
    -- m0a0 + m1a0s + m2(a0s+a1) + m3(a0s+a1)s = 1
    a0s    = ZSqrt5 (toZ (a0 *> p), toZ (a0 *> q))
    a0sa1  = a0s <+ a1
    a0sa1s = ZSqrt5 (-a2,0)

    (Id [1],[n0,n1,n2],_) = toPrincipal (Id [a0,a1,a2]) 

    m0 = n0
    m1 = -n1
    m2 = n1
    m3 = -n2

    -- Finally we get u, v and w:
    u = m0 * a0 +> m2 *> a0sa1
    v = m0 *> a0s <+> m2 *> a0sa1s
    w = m1 * a0 +> m3 *> a0sa1

propPruferDomZSqrt5 :: ZSqrt5 -> ZSqrt5 -> ZSqrt5 -> Property
propPruferDomZSqrt5 x@(ZSqrt5 (a,b)) y@(ZSqrt5 (c,d)) z@(ZSqrt5 (e,f)) = 
  a /= 0 && b /= 0 && c /= 0 && d /= 0 && e /= 0 && f /= 0 ==> propPruferDomain x y z

instance Coherent ZSqrt5 where
  solve = solvePD
