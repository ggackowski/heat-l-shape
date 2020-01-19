-- | The representation of the ring structure.
module Algebra.Structures.Ring 
  ( Ring(..)
  , propAddAssoc, propAddIdentity, propAddInv, propAddComm
  , propMulAssoc, propMulIdentity, propRightDist, propLeftDist
  , propRing
  , (<->), (<^>) -- , (*>), (<*)
  , sumRing, productRing
--  , (~~)
  ) where

import Test.QuickCheck


infixl 8 <^>
infixl 7 <*>
-- infixl 7 *>
-- infixl 7 <*
infixl 6 <+>
infixl 6 <->
--infix  4 ~~

-------------------------------------------------------------------------------
-- | Definition of rings.

class Ring a where
  -- | Addition
  (<+>) :: a -> a -> a

  -- | Multiplication
  (<*>) :: a -> a -> a
  
  -- | Compute additive inverse
  neg   :: a -> a

  -- | The additive identity
  zero  :: a

  -- | The multiplicative identity
  one   :: a


-------------------------------------------------------------------------------
-- Properties

-- | Addition is associative.
propAddAssoc :: (Ring a, Eq a) => a -> a -> a -> (Bool,String)
propAddAssoc a b c = ((a <+> b) <+> c == a <+> (b <+> c), "propAddAssoc")

-- | Zero is the additive identity.
propAddIdentity :: (Ring a, Eq a) => a -> (Bool,String)
propAddIdentity a = (a <+> zero == a && zero <+> a == a, "propAddIdentity")

-- | Negation give the additive inverse.
propAddInv :: (Ring a, Eq a) => a -> (Bool,String)
propAddInv a = (neg a <+> a == zero && a <+> neg a == zero, "propAddInv")

-- | Addition is commutative.
propAddComm :: (Ring a, Eq a) => a -> a -> (Bool,String)
propAddComm x y = (x <+> y == y <+> x, "propAddComm")

-- | Multiplication is associative.
propMulAssoc :: (Ring a, Eq a) => a -> a -> a -> (Bool,String)
propMulAssoc a b c = ((a <*> b) <*> c == a <*> (b <*> c), "propMulAssoc")

-- | Multiplication is right-distributive over addition.
propRightDist :: (Ring a, Eq a) => a -> a -> a -> (Bool,String)
propRightDist a b c = 
  ((a <+> b) <*> c == (a <*> c) <+> (b <*> c), "propRightDist")

-- | Multiplication is left-ditributive over addition.
propLeftDist :: (Ring a, Eq a) => a -> a -> a -> (Bool,String)
propLeftDist a b c = 
 (a <*> (b <+> c) == (a <*> b) <+> (a <*> c), "propLeftDist")

-- | One is the multiplicative identity.
propMulIdentity :: (Ring a, Eq a) => a -> (Bool,String)
propMulIdentity a = (one <*> a == a && a <*> one == a, "propMulIdentity")

-- | Specification of rings. Test that the arguments satisfy the ring axioms.
propRing :: (Ring a, Eq a) => a -> a -> a -> Property
propRing a b c = whenFail (print errorMsg) cond
  where
  (cond,errorMsg) = 
    propAddAssoc a b c &&& propAddIdentity a  &&& propAddInv a        &&&
    propAddComm a b    &&& propMulAssoc a b c &&& propRightDist a b c &&&
    propLeftDist a b c &&& propMulIdentity a

  (False,x) &&& _         = (False,x)
  _         &&& (False,x) = (False,x)
  _         &&& _         = (True,"")


-------------------------------------------------------------------------------
-- Operations

-- | Subtraction
(<->) :: Ring a => a -> a -> a
a <-> b = a <+> neg b

-- | Summation
sumRing :: Ring a => [a] -> a
sumRing = foldr (<+>) zero

-- | Product
productRing :: Ring a => [a] -> a
productRing = foldr (<*>) one

-- | Exponentiation
(<^>) :: Ring a => a -> Integer -> a
x <^> 0 = one
x <^> y = if y < 0 
             then error "<^>: Input should be positive"
             else x <*> x <^> (y-1)

-- | Check if a == b or -a == b or a == -b or -a == -b
-- (~~) :: (Ring a, Eq a) => a -> a -> Bool
-- x ~~ y = x == y || neg x == y || x == neg y || neg x == neg y

{-
-- | Multiply from left with an integer; n *> x means x + x + ... + x, n times.
(*>) :: Ring a => Integer -> a -> a
0 *> _ = zero
n *> x | n > 0     = x <+> x <* (n-1)
       | otherwise = neg (abs n *> x) -- error "<*: Negative input"

-- Multiply from right with an integer.
(<*) :: Ring a => a -> Integer -> a
_ <* 0 = zero
x <* n | n > 0     = x <+> x <* (n-1)
       | otherwise = neg (x <* abs n) -- error "<*: Negative input"
-}       
