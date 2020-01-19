{-# LANGUAGE TypeSynonymInstances #-}
module Z_Examples where

import Test.QuickCheck

import Algebra.Structures.BezoutDomain
import Algebra.Structures.PruferDomain
import Algebra.Structures.StronglyDiscrete
import Algebra.Structures.Coherent
import Algebra.Ideal
import Algebra.Matrix
import Algebra.PLM
import Algebra.Z


-------------------------------------------------------------------------------
-- Bezout domain examples

ex1, ex2 :: (Ideal Z, [Z], [Z])
ex1 = toPrincipal (Id [4,6])
ex2 = toPrincipal (Id [2,3])

ex3, ex4 :: Ideal Z
ex3 = Id [2] `intersectionB` Id [3]
ex4 = Id [2,3] `intersectionB` Id [3]


-------------------------------------------------------------------------------
-- Strong discreteness

ex5 :: Maybe [Z]
ex5 = member 2 ex3


-------------------------------------------------------------------------------
-- Solving equations

ex6 :: Matrix Z
ex6 = solve (Vec [1,2,3])

-- ex7 :: Maybe (Matrix Z, Matrix Z)
ex7 :: Matrix Z
ex7 = solveMxN (M [Vec [1,3,-2], Vec [3,5,6]])

-------------------------------------------------------------------------------
-- PLM

ex8 :: Matrix Z
ex8 = computePLM_B (Id [2,3,4])

-------------------------------------------------------------------------------
-- Prufer domain

ex9 :: (Z,Z,Z)
ex9 = calcUVW 2 3

ex10 :: Matrix Z
ex10 = solvePD (Vec [1,2,3])

-------------------------------------------------------------------------------
-- Chinese remainder theorem

-- Solve the system:
-- x = 12 mod 31
-- x = 20 mod 41
--
-- Every solution x can be written x = 1004 + 1271*n
ex11 :: (Z,Z)
ex11 = crt [12,20] [31,41]
