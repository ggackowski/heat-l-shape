{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | Univariate polynomials parametrised by the variable name.
module Algebra.UPoly
  ( UPoly(..)
  , deg
  , Qx, x
  , toUPoly, monomial
  , lt, deriv
  , cont, isPrimitive
  , toPrimitive, propToPrimitive, gaussLemma
  , gcdUPolyWitness
  , sqfr, sqfrDec
  ) where

import Data.List
import Test.QuickCheck
import Control.Monad (liftM)

import Algebra.TypeChar.Char hiding (Z,Q)
import Algebra.Structures.Field
import Algebra.Structures.FieldOfFractions
import Algebra.Structures.EuclideanDomain
import Algebra.Structures.ExplicitUnits
import Algebra.Structures.BezoutDomain
import Algebra.Structures.GCDDomain
import Algebra.Structures.PruferDomain
-- import Algebra.Structures.StronglyDiscrete

import Algebra.Ideal
import Algebra.Z
import Algebra.Q

-- | Polynomials over a commutative ring, indexed by a phantom type x that
-- denote the name of the variable that the polynomial is over. For example
-- UPoly Q X_ is Q[x] and UPoly Q T_ is Q[t].
newtype CommutativeRing r => UPoly r x = UP [r]
  deriving (Eq,Ord)

-- | The degree of the polynomial.
deg :: CommutativeRing r => UPoly r x -> Integer
deg (UP xs) | length xs < 2 = 0
            | otherwise     = toInteger (length xs) - 1

-- | Useful shorthand for Q[x].
type Qx = UPoly Q X_

-- | The variable x in Q[x].
x :: Qx
x = UP [zero,one]

-- | Take a list and construct a polynomial by removing all zeroes in the end.
toUPoly :: (CommutativeRing r, Eq r) => [r] -> UPoly r x
toUPoly = UP . reverse . dropWhile (==zero) . reverse

isZeroPoly :: CommutativeRing r => UPoly r x -> Bool
isZeroPoly (UP []) = True
isZeroPoly _       = False

-- | Take an element of the ring and the degree of the desired monomial, for
-- example: monomial 3 7 = 3x^7
monomial :: CommutativeRing r => r -> Integer -> UPoly r x
monomial a i = UP $ replicate (fromInteger i) zero ++ [a]

-- | Compute the leading term of a polynomial.
lt :: CommutativeRing r => UPoly r x -> r
lt (UP []) = zero
lt (UP xs) = last xs

constUPoly :: (CommutativeRing r, Eq r) => r -> UPoly r x
constUPoly x = toUPoly [x]

-- | Formal derivative of polynomials in k[x].
deriv :: CommutativeRing r => UPoly r x -> UPoly r x
deriv (UP ps) = UP $ zipWith (\x -> sumRing . replicate x) [1..] (tail ps)

-- | Funny integration:
integrate :: (Enum b, Field b, Integral k, Field k, Fractional b) => UPoly k x -> UPoly b x
integrate (UP ps) = UP $ 0.0 : zipWith (/) (map fromIntegral ps) [1..]

instance (CommutativeRing r, Eq r, Show r, Show x) => Show (UPoly r x) where
  show (UP []) = "0"
  show (UP ps) = init $ fixSign $ concat
                  [ show' (show (undefined :: x)) p n
                  | (p,n) <- zip ps [0..]
                  , p /= zero ]
    where
    show' :: (CommutativeRing r, Show r) => String -> r -> Integer -> String
    show' x p 0 = show p ++ "+"
    show' x p 1 = if p == one then x ++ "+" else show p ++ x ++ "+"
    show' x p n = if p == one
                     then x ++ "^" ++ show n ++ "+"
                     else show p ++ x ++ "^" ++ show n ++ "+"

    fixSign []  = []
    fixSign [x] = [x]
    fixSign ('+':'-':xs) = '-' : fixSign xs
    fixSign (x:xs)       = x : fixSign xs

instance (CommutativeRing r, Eq r, Arbitrary r) => Arbitrary (UPoly r x) where
  arbitrary = liftM (toUPoly . take 5) arbitrary

-- Addition of polynomials.
addUP :: (CommutativeRing r, Eq r) => UPoly r x -> UPoly r x -> UPoly r x
addUP (UP ps) (UP qs) | length ps >= length qs = add' ps qs
                      | otherwise              = add' qs ps
  where add' a b = toUPoly $ zipWith (<+>) a b ++ drop (length b) a

-- Multiplication of polynomials.
mulUP :: (CommutativeRing r, Eq r) => UPoly r x -> UPoly r x -> UPoly r x
mulUP (UP ps) (UP qs) = toUPoly $ m ps qs 0
  where
  m ps qs r | r > length ps + length qs - 2 = []
            | otherwise = c r 0 (length ps-1) (length qs-1) : m ps qs (r+1)

  c (-1) _ _ _ = zero
  c r k m n | r > m || k > n = c (r-1) (k+1) m n
            | otherwise      = ps !! r <*> qs !! k <+> c (r-1) (k+1) m n

instance (CommutativeRing r, Eq r) => Ring (UPoly r x) where
  (<+>)       = addUP
  zero        = UP []
  one         = UP [one]
  neg (UP ps) = UP $ map neg ps
  (<*>)       = mulUP

instance (Show r, Field r, Num r, Show x) => Num (UPoly r x) where
  (+)    = (<+>)
  (-)    = (<->)
  (*)    = (<*>)
  abs    = fromInteger . norm
  signum = undefined -- Is it possible to define this?
  fromInteger x = UP [fromInteger x]

instance (CommutativeRing r, Eq r) => CommutativeRing (UPoly r x) where
instance (CommutativeRing r, Eq r) => IntegralDomain (UPoly r x) where

-- Polynomial rings are Euclidean.
instance (Field k, Eq k) => EuclideanDomain (UPoly k x) where
  norm (UP ps)             = fromIntegral (length ps) - 1
  quotientRemainder f g = qr zero f
    where
    -- This is the division algorithm in k[x]. Page 39 in Cox.
    qr q r | norm g <= norm r = 
              qr (q <+> monomial (lt r </> lt g) (norm r - norm g))
                 (r <-> monomial (lt r </> lt g) (norm r - norm g) <*> g)
           | otherwise = (q,r)

instance (Field k, Eq k) => PruferDomain (UPoly k x) where
  calcUVW = calcUVW_B

instance (ExplicitUnits a, Eq a) => ExplicitUnits (UPoly a x) where
  unit (UP [a]) = case unit a of
    Just a' -> Just (UP [a'])
    Nothing -> Nothing
  unit _        = Nothing

-- Now that we know that the polynomial ring k[x] is a Bezout domain it is
-- possible to implement membership in an ideal of k[x]. f is a member of the
-- ideal <f1,...,fn> if the rest is zero when dividing f with the principal
-- ideal <h>.
-- instance (Field k, Eq k, Show x) => StronglyDiscrete (UPoly k x) where
--  member p ps = modulo p h == zero
--    where Id [h] = (\(a,_,_) -> a) $ toPrincipal ps


-------------------------------------------------------------------------------
-- Proof that if A is a GCD domain then A[x] is a GCD domain following
-- section 4.4 in A course in constructive algebra.

-- Some test polynomials:

test1 :: UPoly Z X_
test1 = toUPoly [1,2,3]

test2 :: UPoly Z X_
test2 = toUPoly [2,4,6,8,10]

test3 :: UPoly Q X_
test3 = toUPoly [inv 2, inv 3, inv 4]


-- | Compute the content of a polynomial, i.e. the gcd of the coefficients.
cont :: (GCDDomain a, Eq a) => UPoly a x -> a
cont (UP xs) = case filter (/= zero) xs of
  []  -> error "cont: Can't compute the content of the zero polynomial"
  xs' -> ggcd xs'

-- *Algebra.UPoly> cont test1
-- 1
-- *Algebra.UPoly> cont test2
-- 2


-- | If all coefficients are relatively prime then the polynomial is primitive.
isPrimitive :: (ExplicitUnits a, GCDDomain a, Eq a) => UPoly a x -> Bool
isPrimitive = isUnit . cont

-- *Algebra.UPoly> isPrimitive test1
-- True
-- *Algebra.UPoly> isPrimitive test2
-- False


-- | Lemma 4.2: Given a polynomial p in K[x] where K=Quot(A) we can find c in K
-- and q primitive in A[x] such that p = cq.
toPrimitive :: (GCDDomain a, Eq a)
            => UPoly (FieldOfFractions a) x
            -> (FieldOfFractions a, UPoly a x)
toPrimitive p@(UP xs) = (c,q)
  where
  c0' = toFieldOfFractions $ productRing $ map denominator xs
  c0  = inv c0'
  g0  = map (fromFieldOfFractions . (c0' <*>)) xs
  cg0 = toFieldOfFractions $ cont $ UP g0
  c   = c0 <*> cg0
  q   = toUPoly $ map (\x -> fromFieldOfFractions (toFieldOfFractions x </> cg0)) g0

-- *Algebra.UPoly> toPrimitive test3
-- (1/12,6+4x+3x^2)

-- Specification of toPrimitive.
propToPrimitive :: (ExplicitUnits a, GCDDomain a, Eq a) => UPoly (FieldOfFractions a) x -> Property
propToPrimitive p =
  not (isZeroPoly p) ==>
    p == toUPoly (map (\x -> c <*> toFieldOfFractions x) q) && isPrimitive (UP q)
  where (c,UP q) = toPrimitive p

-- *Algebra.UPoly> quickCheck (propToPrimitive :: UPoly Q X_ -> Property)
-- +++ OK, passed 100 tests.


-- | Gauss lemma says that if p and q are polynomials over a GCD domain then
-- cont(pq) = cont(p) * cont(q).
gaussLemma :: (ExplicitUnits a, GCDDomain a, Eq a) => UPoly a x -> UPoly a x -> Property
gaussLemma p q =
  not (isZeroPoly p) && not (isZeroPoly q) ==>
    cont (p <*> q) ~= cont p <*> cont q

-- *Algebra.UPoly> quickCheck (gaussLemma :: UPoly Z X_ -> UPoly Z X_ -> Property)
-- +++ OK, passed 100 tests.

liftUPoly :: (GCDDomain a, Eq a) => UPoly a x -> UPoly (FieldOfFractions a) x
liftUPoly (UP xs) = toUPoly $ map toFieldOfFractions xs

-- | Proof that if A is a GCD domain then A[x] also is a GCD domain. This also
-- computes witnesses that the computed GCD divides the given polynomials.
gcdUPolyWitness :: (GCDDomain a, Eq a)
                => UPoly a x
                -> UPoly a x
                -> (UPoly a x, UPoly a x, UPoly a x)
gcdUPolyWitness p q = (constUPoly d <*> h, constUPoly x <*> a, constUPoly y <*> b)
  where
  (h',a',b') = gcd' (liftUPoly p) (liftUPoly q)

  (_,h) = toPrimitive h'
  (_,a) = toPrimitive a'
  (_,b) = toPrimitive b'

  (d,x,y) = gcd' (cont p) (cont q)

test4, test5 :: UPoly Z X_
test4 = toUPoly [6,7,1]
test5 = toUPoly [-6,-5,1]

-- *Algebra.UPoly> gcdUPolyWitness test4 test5
-- (1+x,6+x,-6+x)
-- *Algebra.UPoly> gcdUPolyWitness test4 test4
-- (6+7x+x^2,1,1)
-- *Algebra.UPoly> gcdUPolyWitness test1 test2
-- (1,1+2x+3x^2,2+4x+6x^2+8x^3+10x^4)

-- This does not work:
-- instance (GCDDomain a, Eq a) => GCDDomain (UPoly a x) where
--   gcd' = gcdUPolyWitness


-------------------------------------------------------------------------------
-- Square free decomposition.
-- Teo Mora; Solving Polynomial Equations Systems I. pg 69-70
-- Works only for Char 0
--TODO: Add check for char
--square free associate of f
-- | Square free decomposition of a polynomial.
sqfr :: (Num k, Field k) => UPoly k x -> UPoly k x
sqfr f = f `quotient` euclidAlg f f'
  where f' = deriv f

-- | Distinct power factorization, aka square free decomposition
sqfrDec :: (Num k, Field k) => UPoly k x -> [UPoly k x]
sqfrDec f = help p q
  where
  p = euclidAlg f (deriv f)
  q = f `quotient` p

  help p q | norm q < 1    = []
           | otherwise  = t : help (p `quotient` s) s
    where
    s = euclidAlg p q
    t = q `quotient` s

-- | Pseudo-division of polynomials.
--
-- Given s(x) and p(x) compute c, q(x) and r(x) such that:
--
--   cs(x) = p(x)q(x)+r(x), deg r < deg p.
pseudoDivide :: (CommutativeRing a, Eq a)
             => UPoly a x -> UPoly a x -> (a, UPoly a x, UPoly a x)
pseudoDivide s p
  | m < n     = (one,zero,s)
  | otherwise = pD (a' <*> s' <-> b' <*> xmn <*> p') 1 (b' <*> xmn) s2
  where
  n = deg p
  m = deg s

  a   = lt p
  a'  = constUPoly a
  b   = lt s
  b'  = constUPoly b
  s'  = s <-> monomial b m
  xmn = monomial one (m-n)
  p'  = p <-> monomial a n
  s2  = a' <*> s' <-> b' <*> xmn <-> p'

  pD s k out1 out2
    | deg s < n = (a <^> k,out1,out2)
    | otherwise = pD s3 (k+1) (b2xm2n <+> a' <*> out1) s3
    where
    b2  = lt s
    m2  = deg s
    s2' = s <-> monomial b2 m2
    b2xm2n = monomial b2 (m2-n)
    s3 = (a' <*> s) <-> (b2xm2n <*> p)


propPD :: (CommutativeRing a, Eq a) => UPoly a x -> UPoly a x -> Property
propPD s p = deg s > 1 && deg p > 1 ==> constUPoly c <*> s == p <*> q <+> r
  where (c,q,r) = pseudoDivide s p
