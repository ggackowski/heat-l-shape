{-# LANGUAGE TypeSynonymInstances #-}
-- | The field of rational functions is the field of fractions of k[x].
module Algebra.FieldOfRationalFunctions 
  ( FieldOfRationalFunctions(..)
  , QX, toQX, toQx
  ) where

import Test.QuickCheck

import Algebra.Structures.Field
import Algebra.Structures.FieldOfFractions
import Algebra.UPoly
import Algebra.Q
import Algebra.TypeChar.Char (X_)


-------------------------------------------------------------------------------
-- | Field of rational functions.

type FieldOfRationalFunctions k x = FieldOfFractions (UPoly k x)

-- | The field of fraction of Q[x].
type QX = FieldOfRationalFunctions Q X_

toQX :: Qx -> QX
toQX = toFieldOfFractions

toQx :: QX -> Qx
toQx = fromFieldOfFractions

propFieldQX :: QX -> QX -> QX -> Property
propFieldQX = propField

-- k(x) Num.
instance (Show k, Field k, Num k, Show x) => Num (FieldOfRationalFunctions k x) where
  (+)    = (<+>)
  (-)    = (<->)
  (*)    = (<*>)
  fromInteger x = toFieldOfFractions $ UP [fromInteger x]
  signum = undefined
  abs    = undefined
