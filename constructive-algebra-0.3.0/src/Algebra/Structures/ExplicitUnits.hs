-- | Structure of rings with explicit units. 
module Algebra.Structures.ExplicitUnits
  ( ExplicitUnits(..)
  , propUnit, isUnit, (%|), (~=)
  ) where 

import Algebra.Structures.IntegralDomain
import Algebra.Structures.GCDDomain

infix 5 %|
infix 4 ~=

-- | A ring has explicit units if there is a function that can test if an
-- element is invertible and if this is the case give the inverse. 
class IntegralDomain a => ExplicitUnits a where
  unit :: a -> Maybe a

propUnit :: (ExplicitUnits a, Eq a) => a -> Bool
propUnit a = case unit a of
  Just a' -> a <*> a' == one
  Nothing -> True

-- | An element is a unit if it is invertible. 
isUnit :: ExplicitUnits a => a -> Bool
isUnit = maybe False (const True) . unit

-- | Decidable units is sufficient to decide divisibility in GCD domains. 
(%|) :: (ExplicitUnits a, GCDDomain a) => a -> a -> Bool
a %| b = let (g,x,y) = gcd' a b
         in isUnit x

-- | Test for associatedness, i.e. a ~ b iff a | b /\\ b | a.
(~=) :: (ExplicitUnits a, GCDDomain a) => a -> a -> Bool
a ~= b = a %| b && b %| a 
