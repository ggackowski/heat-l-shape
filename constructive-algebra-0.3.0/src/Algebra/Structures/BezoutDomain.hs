{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- | Representation of Bezout domains. That is non-Noetherian analogues of
-- principal ideal domains. This means that all finitely generated ideals are
-- principal.
--
module Algebra.Structures.BezoutDomain
  ( BezoutDomain(..), propBezoutDomain
  , toPrincipal, propToPrincipal, propIsSameIdeal
  , gcdB, intersectionB, intersectionBWitness
  , solveB, crt
  ) where

import Test.QuickCheck

import Algebra.Structures.IntegralDomain
import Algebra.Structures.Coherent
import Algebra.Structures.EuclideanDomain
import Algebra.Structures.StronglyDiscrete
import Algebra.Matrix
import Algebra.Ideal


-------------------------------------------------------------------------------
{- | Bezout domains

Has a Bezout function which given a and b give g, a1, b1, x and y such that:

 - g = gcd(a,b)

 - a = g * a1 and b = g * b1

 - g = a * x + b * y

-}
class IntegralDomain a => BezoutDomain a where
  bezout :: a -> a -> (a,a,a,a,a)

propBezoutDomain :: (BezoutDomain a, Eq a) => a -> a -> Property
propBezoutDomain a b = 
  let (g,a1,b1,x,y) = bezout a b
  in if a == g <*> a1 && b == g <*> b1 && a <*> x <+> b <*> y == g
        then propIntegralDomain a b b
        else whenFail (print "propBezoutDomain") False


{- | Compute a principal ideal from another ideal. Also give witness that the
principal ideal is equal to the first ideal.

toPrincipal \<a_1,...,a_n> = (\<a>,u_i,v_i)
  where

  sum (u_i * a_i) = a

  a_i = v_i * a
-}
toPrincipal :: BezoutDomain a => Ideal a -> (Ideal a,[a],[a])
toPrincipal (Id [])    = error "toPrincipal: Empty input"
toPrincipal (Id [a])   = (Id [a],[one],[one])
toPrincipal (Id [a,b]) = 
  let (g,a1,b1,x,y) = bezout a b
  in (Id [g],[x,y],[a1,b1])
toPrincipal (Id (a:xs)) = 
  let (Id [g],us,vs) = toPrincipal (Id xs)
      (g',a1,b1,x,y) = bezout a g
  in (Id [g'],x : map (y <*>) us,a1 : map (b1 <*>) vs)

-- | Test that the generated ideal is principal.
propToPrincipal :: (BezoutDomain a, Eq a) => Ideal a -> Bool
propToPrincipal = isPrincipal . (\(a,_,_) -> a) . toPrincipal

-- | Test that the generated ideal generate the same elements as the given.
propIsSameIdeal :: (BezoutDomain a, Eq a) => Ideal a -> Bool
propIsSameIdeal (Id as) =
  let (Id [a], us, vs) = toPrincipal (Id as)
  in a == foldr1 (<+>) (zipWith (<*>) as us)
  && and [ ai == a <*> vi | (ai,vi) <- zip as vs ]
  && length us == l_as && length vs == l_as
  where l_as = length as

-- TODO: Add error cases...
gcdB :: BezoutDomain a => a -> a -> a
gcdB a b = g
  where (Id [g],_,_) = toPrincipal (Id [a,b])

-------------------------------------------------------------------------------
-- Euclidean domain -> Bezout domain

instance (EuclideanDomain a, Eq a) => BezoutDomain a where
  bezout a b
    | b == zero        = (a,one,zero,one,zero)
    | a == zero        = (b,zero,one,zero,one)
    | norm a <= norm b = let (q,r)           = quotientRemainder b a
                             (g,a1,r1,u',v') = bezout a r
                         in (g,a1,r1<+>(q<*>a1),u'<->(q<*>v'),v')
    | otherwise        = let (q,r)           = quotientRemainder a b
                             (g,b1,r1,u',v') = bezout b r
                         in (g,r1<+>(q<*>b1),b1,v',u'<->(v'<*>q))


-------------------------------------------------------------------------------
-- | Intersection of ideals with witness.
--
-- If one of the ideals is the zero ideal then the intersection is the zero
-- ideal.

intersectionBWitness :: (BezoutDomain a, Eq a)
              => Ideal a
              -> Ideal a
              -> (Ideal a, [[a]], [[a]])
intersectionBWitness (Id xs) (Id ys)
  | xs' == [] = zeroIdealWitnesses xs ys
  | ys' == [] = zeroIdealWitnesses xs ys
  | otherwise = (Id [l], [handleZero xs as], [handleZero ys bs])
  where
  xs'            = filter (/= zero) xs
  ys'            = filter (/= zero) ys

  (Id [a],us1,vs1) = toPrincipal (Id xs')
  (Id [b],us2,vs2) = toPrincipal (Id ys')

  (Id [g],[u1,u2],[v1,v2]) = toPrincipal (Id [a,b])

  l  = g <*> v1 <*> v2
  as = map (v2 <*>) us1
  bs = map (v1 <*>) us2

-- Handle the zeroes specially. If the first element in xs is a zero
-- then the witness should be zero otherwise use the computed witness.
handleZero :: (Ring a, Eq a) => [a] -> [a] -> [a]
handleZero xs []
  | all (==zero) xs = xs
  | otherwise       = error "intersectionB: This should be impossible"
handleZero (x:xs) (a:as)
  | x == zero = zero : handleZero xs (a:as)
  | otherwise = a    : handleZero xs as
handleZero [] _  = error "intersectionB: This should be impossible"


-- | Intersection without witness.
intersectionB :: (BezoutDomain a, Eq a) => Ideal a -> Ideal a -> Ideal a
intersectionB a b = (\(x,_,_) -> x) $ intersectionBWitness a b


-------------------------------------------------------------------------------
-- | Coherence of Bezout domains.
solveB :: (BezoutDomain a, Eq a) => Vector a -> Matrix a
solveB x = solveWithIntersection x intersectionBWitness

-- instance (BezoutDomain r, Eq r) => Coherent r where
--   solve x = solveWithIntersection x intersectionB


-------------------------------------------------------------------------------
-- | Strongly discreteness for Bezout domains
--
-- Given x, compute as such that x = sum (a_i * x_i)
--
instance (BezoutDomain a, Eq a) => StronglyDiscrete a where
  member x (Id xs) | x == zero = Just (replicate (length xs) zero)
                   | otherwise = if a == g
                                    then Just witness
                                    else Nothing
    where
    -- (<g>, as, bs)   = <x1,...,xn>
    -- sum (a_i * x_i) = g
    -- x_i             = b_i * g
    (Id [g], as, bs) = toPrincipal (Id (filter (/= zero) xs))
    (Id [a], _,[q1,q2]) = toPrincipal (Id [x,g])

    -- x = qg = q (sum (ai * xi)) = sum (q * ai * xi)
    witness = handleZero xs (map (q1 <*>) as)


-------------------------------------------------------------------------------
-- | Chinese remainder theorem
--
-- Given a_1,...,a_n and m_1,...,m_n such that gcd(m_i,m_j) = 1.
-- Let m = m_1*...*m_n compute a such that:
--
-- (1) a = a_i (mod m_i)
--
-- (2) If b is such that b = a_i (mod m_i) then a = b (mod m)
--
-- The function return (a,m).

crt :: (BezoutDomain a, Eq a) => [a] -> [a] -> (a,a)
crt as ms
  | length as /= length ms = error "crt: Input lists need to have same length"
  | not (and [ gcdB m1 m2 == one | m1 <- ms, m2 <- ms, m1 /= m2 ]) =
      error "crt: All ms need to be relatively prime"
  | otherwise = crt' as ms
  where
  m = productRing ms

  crt' :: (BezoutDomain a, Eq a) => [a] -> [a] -> (a,a)
  crt' [] []                 = error "crt: Empty input"
  crt' [a] [m]               = (a,m)
  crt' [a1,a2] [m1,m2]       = let (_,[c1,c2],_) = toPrincipal (Id [m1,m2])
                               in (a1 <+> m1 <*> c1 <*> (a2 <-> a1), m1 <*> m2)
  crt' (a1:a2:as) (m1:m2:ms) = let (a',m') = crt' [a1,a2] [m1,m2]
                               in crt' (a':as) (m':ms)
