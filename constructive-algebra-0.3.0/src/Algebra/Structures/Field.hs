-- | Structure for fields.
module Algebra.Structures.Field
  ( module Algebra.Structures.IntegralDomain
  , Field(inv)
  , propMulInv, propField
  , (</>)
  ) where

import Test.QuickCheck

import Algebra.Structures.Ring
import Algebra.Structures.IntegralDomain

infixl 7 </>

-------------------------------------------------------------------------------
-- | Definition of fields.

class IntegralDomain a => Field a where
  inv :: a -> a

propMulInv :: (Field a, Eq a) => a -> Bool
propMulInv a = a == zero || inv a <*> a == one

-- | Specification of fields. Test that the multiplicative inverses behave as 
-- expected and that it satisfies the axioms of integral domains.
propField :: (Field a, Eq a) => a -> a -> a -> Property
propField a b c = if propMulInv a
                     then propIntegralDomain a b c 
                     else whenFail (print "propMulInv") False

-------------------------------------------------------------------------------
-- Operations


-- | Division
(</>) :: Field a => a -> a -> a
x </> y = x <*> inv y
