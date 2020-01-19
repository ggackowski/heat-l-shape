{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The elliptic curve y^2 = 1 - x^4 in Q[x,y].
module Algebra.EllipticCurve (EllipticCurve(..)) where

import Test.QuickCheck

import Algebra.Structures.Field -- hiding ((<*), (*>))
import Algebra.Structures.EuclideanDomain (quotient, genEuclidAlg)
import Algebra.Structures.BezoutDomain (toPrincipal)
import Algebra.Structures.PruferDomain
import Algebra.Structures.Coherent
import Algebra.FieldOfRationalFunctions
import Algebra.Ideal
import Algebra.UPoly

-- | The elliptic curve y^2=1-x^4 over Q[x,y].
newtype EllipticCurve = C (Qx,Qx)
  deriving (Eq,Arbitrary)

instance Show EllipticCurve where
  show (C (a,b)) | a == zero && b == zero = "0"
                 | a == zero              = show b ++ "*y"
                 | b == zero              = show a
                 | otherwise = case show b of 
                   ['-','1'] -> show a ++ "-y"
                   ('-':xs)  -> show a ++ "-" ++ xs ++ "*y"
                   xs        -> show a ++ "+" ++ xs ++ "*y" 

-- Arithmetical properties
instance Ring EllipticCurve where
  (C (a,b)) <+> (C (c,d)) = C (a + c, b + d)
  (C (a,b)) <*> (C (c,d)) = C (a*c + b*d*(1-x^4), a*d + b*c)
  neg (C (a,b))           = C (neg a, neg b)
  zero                    = C (zero,zero)
  one                     = C (one,zero)
  
instance CommutativeRing EllipticCurve where

instance IntegralDomain EllipticCurve where

propIntDomEC :: EllipticCurve -> EllipticCurve-> EllipticCurve -> Property
propIntDomEC = propIntegralDomain

--------------------------------------------------------------------------------
-- Useful auxiliary functions:

(*>), (+>) :: Qx -> EllipticCurve -> EllipticCurve
r *> (C (a,b)) = C (r*a,r*b)
r +> (C (a,b)) = C (r+a,b)

(<*), (<+) :: EllipticCurve -> Qx -> EllipticCurve
(C (a,b)) <* r = C (a*r,b*r)
(C (a,b)) <+ r = C (a+r,b)

infixl 7 *>, <*
infixl 6 +>, <+

--------------------------------------------------------------------------------

instance PruferDomain EllipticCurve where
  calcUVW (C (a,b)) (C (c,d)) = (u,v,w)
    where
    p = toQX (a * c - b * d * (1 - x^4)) </> toQX (c^2 - d^2 * (1 - x^4))
    q = toQX (b * c - a * d) </> toQX (c^2 - d^2 * (1 - x^4))

    s :: (QX,QX) 
    s = (p,q)

    -- a0's^2 + a1's + a2' = 0
    a0' = (c^2 - d^2 * (1 - x^4))^2
    a1' = -2 * (a * c - b * d * (1 - x^4)) * (c^2 - d^2 * (1 - x^4))
    a2' = (a * c - b * d * (1 - x^4))^2 - ((b * c - a * d)^2 * (1-x^4))

    -- Make <a0,a1,a2> = 1
    g  = genEuclidAlg [a0',a1',a2']
    a0 = a0' `quotient` g
    a1 = a1' `quotient` g
    a2 = a2' `quotient` g

    -- n0 * a0 + n1 * a1 + n2 * a2 = 1
    (Id [g'],[n0,n1,n2],_) = toPrincipal (Id [a0,a1,a2])

    a0s    = case s of
      (p,q) -> C (toQx (a0' <*> p), toQx (a0' <*> q))
      where a0' = toQX a0
    a0sa1  = a0s <+ a1
    a0sa1s = C (neg a2,zero)

    alpha = a0s 
    beta  = a0sa1s

    m0 = n0
    m1 = -n1
    m2 = n1
    m3 = -n2

    u = m0 * a0 +> m2 *> a0sa1
    v = m0 *> alpha <+> m2 *> beta
    w = m1 * a0 +> m3 *> a0sa1

instance Coherent EllipticCurve where
  solve = solvePD

-- Properties:
propPruferDomEC :: EllipticCurve -> EllipticCurve -> EllipticCurve -> Property
propPruferDomEC x@(C (a,b)) y@(C (c,d)) z@(C (e,f)) = 
  a /= zero && b /= zero && c /= zero && d /= zero && e /= zero && f /= zero 
  ==> propPruferDomain x y z

propIntersectionPEC :: Ideal EllipticCurve -> Ideal EllipticCurve -> Property
propIntersectionPEC i@(Id is) j@(Id js) = 
  length is <= 5 && length js <= 5 ==> isSameIdeal intersectionPDWitness i j
