{-# LANGUAGE TypeSynonymInstances #-}
module Algebra.Z 
  ( Z
  , IntegralDomain(..)
  ) where

import Test.QuickCheck

import Algebra.Structures.IntegralDomain
import Algebra.Structures.EuclideanDomain
import Algebra.Structures.ExplicitUnits
import Algebra.Structures.StronglyDiscrete
import Algebra.Structures.BezoutDomain
import Algebra.Structures.GCDDomain
import Algebra.Structures.PruferDomain
import Algebra.Structures.Coherent
import Algebra.Ideal
import Algebra.Matrix
import Algebra.PLM


-- | Type synonym for integers.
type Z = Integer

instance Ring Z where
  (<*>) = (*)
  (<+>) = (+)
  neg   = negate
  one   = 1
  zero  = 0

instance CommutativeRing Z

instance IntegralDomain Z

propIntegralDomainZ :: Z -> Z -> Z -> Property
propIntegralDomainZ = propIntegralDomain

instance ExplicitUnits Z where
  unit 1    = Just 1
  unit (-1) = Just (-1)
  unit _    = Nothing

instance EuclideanDomain Z where
  norm = abs
  quotientRemainder = quotRem

propEuclideanDomainZ :: Z -> Z -> Z -> Property
propEuclideanDomainZ = propEuclideanDomain

-- Euclidean domain -> Bezout domain
propBezoutDomainZ :: Z -> Z -> Property
propBezoutDomainZ = propBezoutDomain

propToPrincipalZ :: Ideal Z -> Bool
propToPrincipalZ = propToPrincipal

propIsSameIdealZ :: Ideal Z -> Bool
propIsSameIdealZ = propIsSameIdeal

-- Bezout domain -> Strongly discrete
propStronglyDiscreteZ :: Z -> Ideal Z -> Bool
propStronglyDiscreteZ = propStronglyDiscrete

-- Bezout domain -> Coherent
instance Coherent Z where
  solve = solveB

propCoherentZ :: Vector Z -> Bool
propCoherentZ = propCoherent

propSolveMxNZ :: Matrix Z -> Bool
propSolveMxNZ = propSolveMxN

propSolveGeneralEquationZ :: Vector Z -> Z -> Bool
propSolveGeneralEquationZ = propSolveGeneralEquation

-- Not working perfectly...
propSolveGeneralZ :: Matrix Z -> Vector Z -> Property
propSolveGeneralZ = propSolveGeneral


-- GCD Domain
propGCDDomainZ :: Z -> Z -> Z -> Property
propGCDDomainZ = propGCDDomain

-- PLM
propPLMZ :: Ideal Z -> Bool
propPLMZ id = propPLM id (computePLM_B id)

-- Prufer domain
instance PruferDomain Z where
  calcUVW = calcUVW_B

propPruferDomainZ :: Z -> Z -> Z -> Property
propPruferDomainZ = propPruferDomain

propIsSameIdealPruferDomain :: Ideal Z -> Ideal Z -> Bool
propIsSameIdealPruferDomain = isSameIdeal intersectionPDWitness
