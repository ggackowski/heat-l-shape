{-# LANGUAGE TypeSynonymInstances #-}
-- | Representation of rational numbers as the field of fractions of Z.
module Algebra.Q ( Q, toQ, toZ ) where

import Data.Ratio (numerator, denominator)
import Test.QuickCheck

import Algebra.Structures.Field
import Algebra.Structures.FieldOfFractions hiding (numerator, denominator)
import Algebra.Structures.ExplicitUnits
import Algebra.Z

-------------------------------------------------------------------------------
-- | Q is the field of fractions of Z.

type Q = FieldOfFractions Z

instance Num Q where
  (+)              = (<+>)
  (*)              = (<*>)
  abs (F (a,b))    = F (abs a, b) 
  signum (F (a,_)) = F (signum a,one)
  fromInteger      = toQ

instance Fractional Q where
  (/)            = (</>)
  fromRational x = 
    reduce $ F (fromIntegral (numerator x), fromIntegral (denominator x))

toQ :: Z -> Q
toQ = toFieldOfFractions

toZ :: Q -> Z
toZ = fromFieldOfFractions

propFieldQ :: Q -> Q -> Q -> Property
propFieldQ = propField

instance ExplicitUnits Q where
  unit a = if a == 0 then Nothing else Just (inv a)
