{-# LANGUAGE TypeSynonymInstances #-}
-- | A small simple matrix library.
module Algebra.Matrix
  ( Vector(Vec)
  , unVec, lengthVec
  , Matrix(M), matrix
  , matrixToVector, vectorToMatrix, unMVec, unM, (!!!)
  , identity, propLeftIdentity, propRightIdentity
  , mulM, addM, transpose, isSquareMatrix, dimension
  , scale, swap, pivot
  , addRow, subRow, addCol, subCol
  , findPivot, forwardElim, gaussElim, gaussElimCorrect
  ) where

import qualified Data.List as L
import Data.Function (on)
import Control.Monad (liftM)
import Control.Arrow hiding ((<+>))
import Test.QuickCheck

import Algebra.Structures.Field

import Debug.Trace

-------------------------------------------------------------------------------
-- | Row vectors

newtype Vector r = Vec [r] deriving (Eq)

instance Show r => Show (Vector r) where
  show (Vec vs) = show vs

-- Generate vector of length 1-10
instance Arbitrary r => Arbitrary (Vector r) where
  arbitrary = do n <- choose (1,10) :: Gen Int
                 liftM Vec $ gen n
    where
    gen 0 = return []
    gen n = do x <- arbitrary
               xs <- gen (n-1)
               return (x:xs)

instance Functor Vector where 
  fmap f = Vec . map f . unVec

{-
instance Ring r => Ring (Vector r) where
  (Vec xs) <+> (Vec ys) | length xs == length ys = Vec (zipWith (<+>) xs ys)
                        | otherwise = error "Bad dimensions in vector addition"
  (Vec xs) <*> (Vec ys) | length xs == length ys = Vec (zipWith (<*>) xs ys)
                        | otherwise = error "Bad dimensions in vector multiplication"
  -- In order to do these we need to know the length of the vector in advance...
  -- Give me dependent types!
  one  = ?
  zero = ?
-}

unVec :: Vector r -> [r]
unVec (Vec vs) = vs

lengthVec :: Vector r -> Int
lengthVec = length . unVec


-------------------------------------------------------------------------------
-- | Matrices

newtype Matrix r = M [Vector r]
  deriving (Eq)

instance Show r => Show (Matrix r) where
  show xs = case unlines (map show (unMVec xs)) of
    [] -> "[]" 
    xs -> init xs ++ "\n"

-- Generate matrices with at most 10 rows
instance Arbitrary r => Arbitrary (Matrix r) where
  arbitrary = do n <- choose (1,10) :: Gen Int
                 m <- choose (1,10) :: Gen Int
                 xs <- sequence [ liftM Vec (gen n) | _ <- [1..m]]
                 return (M xs)
    where
    gen 0 = return []
    gen n = do x <- arbitrary
               xs <- gen (n-1)
               return (x:xs)

instance Functor Matrix where
  fmap f = M . map (fmap f) . unM

-- | Construct a mxn matrix.
matrix :: [[r]] -> Matrix r
matrix xs = 
  let m = fromIntegral $ length xs
      n = fromIntegral $ length (head xs)
  in if length (filter (\x -> fromIntegral (length x) == n) xs) == length xs 
        then M (map Vec xs)
        else error "matrix: Bad dimensions"

unM :: Matrix r -> [Vector r]
unM (M xs) = xs

unMVec :: Matrix r -> [[r]]
unMVec = map unVec . unM

vectorToMatrix :: Vector r -> Matrix r
vectorToMatrix = matrix . (:[]) . unVec

matrixToVector :: Matrix r -> Vector r
matrixToVector m | fst (dimension m) == 1 = head (unM m)
                 | otherwise              = error "matrixToVector: Bad dimension"

(!!!) :: Matrix a -> (Int,Int) -> a
m !!! (r,c) | r >= 0 && r < rows && c >= 0 && c < cols = unMVec m !! r !! c
            | otherwise = error "!!!: Out of bounds"
  where
  (rows,cols) = dimension m 

-- | Compute the dimension of a matrix.
dimension :: Matrix r -> (Int, Int)
dimension (M xs) | null xs   = (0,0)
                 | otherwise = (length xs, length (unVec (head xs)))


isSquareMatrix :: Matrix r -> Bool
isSquareMatrix (M xs) = all (== length xs) (map lengthVec xs)

-- | Transpose a matrix.
transpose :: Matrix r -> Matrix r
transpose (M xs) = matrix (L.transpose (map unVec xs))

-- | Matrix addition.
addM :: Ring r => Matrix r -> Matrix r -> Matrix r
addM (M xs) (M ys)
  | dimension (M xs) == dimension (M ys) = m
  | otherwise = error "Bad dimensions in matrix addition"
  where
  m = matrix (zipWith (zipWith (<+>)) (map unVec xs) (map unVec ys))

-- | Matrix multiplication.
mulM :: Ring r => Matrix r -> Matrix r -> Matrix r
mulM (M xs) (M ys)
  | snd (dimension (M xs)) == fst (dimension (M ys)) = m
  | otherwise = error "Bad dimensions in matrix multiplication"
    where
    m = matrix [ [ mulVec x y | y <- L.transpose (map unVec ys) ]
               | x <- map unVec xs ]

mulVec xs ys | length xs == length ys = foldr (<+>) zero $ zipWith (<*>) xs ys
             | otherwise = error "mulVec: Bad dimension"

{-
-- In order to do this the size of the matrix need to be encoded in the type
-- There is also a problem with the fact that it is not possible to add or
-- multiply matrices with bad dimensions, so the generation of matrices has to be better...
instance Ring r => Ring (Matrix r) where
  (<+>) = add
  (<*>) = mul
  neg (Vec xs d) = Vec [ map neg x | x <- xs ] d
  zero  = undefined
-}

-- | Construct a nxn identity matrix.
identity :: IntegralDomain r => Int -> Matrix r
identity n = matrix (xs 0)
  where
  xs x | x == n    = []
       | otherwise = (replicate x zero ++ [one] ++
                      replicate (n-x-1) zero) : xs (x+1)

-- Specification of identity.
propLeftIdentity :: (IntegralDomain r, Eq r) => Matrix r -> Bool
propLeftIdentity a = a == identity n `mulM` a
  where n = fst (dimension a)

propRightIdentity :: (IntegralDomain r, Eq r) => Matrix r -> Bool
propRightIdentity a = a == a `mulM` identity m
  where m = snd (dimension a)


-------------------------------------------------------------------------------
-- Operations on matrices.

-- | Scale a row in a matrix.
scale :: CommutativeRing a => Matrix a -> Int -> a -> Matrix a
scale m r s
  | 0 <= r && r < rows = matrix $ take r m' ++ map (s <*>) (m' !! r) : drop (r+1) m'
  | otherwise = error "scale: Index out of bounds"
  where
  (rows,_) = dimension m
  m'       = unMVec m

-- Scaling does not affect dimension
propScaleDimension :: (Arbitrary r, CommutativeRing r) => Matrix r -> Int -> r -> Bool
propScaleDimension m r s = d == dimension (scale m (mod r rows) s)
  where d@(rows,_) = dimension m

-- | Swap two rows of a matrix.
swap :: Matrix a -> Int -> Int -> Matrix a
swap m i j
  | 0 <= i && i <= r && 0 <= j && j <= r = matrix $ swap' m' i j
  | otherwise = error "swap: Index out of bounds"
  where
  (r,_) = dimension m
  m'    = unMVec m

  swap' xs 0 0     = xs
  swap' (x:xs) 0 j = (x:xs) !! j : take (j-1) xs ++ x : drop j xs
  swap' xs i 0     = swap' xs 0 i
  swap' (x:xs) i j = x : swap' xs (i-1) (j-1)

-- Swapping does not affect dimension
propSwapDimension :: Matrix () -> Int -> Int -> Bool
propSwapDimension m i j = d == dimension (swap m (mod i r) (mod j r))
  where d@(r,_) = dimension m

-- Swap is itselfs identity.
propSwapIdentity :: Matrix () -> Int -> Int -> Bool
propSwapIdentity m i j = m == swap (swap m i' j') i' j'
  where
  d@(r,_) = dimension m
  i'      = mod i r
  j'      = mod j r


-- Add the row-vector to the specified row of the matrix.
addRow :: CommutativeRing a => Matrix a -> Vector a -> Int -> Matrix a
addRow m row@(Vec xs) x
  | 0 <= x && x < r = matrix $ take x m' ++
                               zipWith (<+>) (m' !! x) xs :
                               drop (x+1) m'
  | c /= length xs  = error "addRow: Bad length of row"
  | otherwise       = error "addRow: Bad row number"
    where
    (r,c) = dimension m
    m'    = unMVec m

propAddRowDimension :: (CommutativeRing a, Arbitrary a)
                    => Matrix a -> Vector a -> Int -> Property
propAddRowDimension m row@(Vec xs) r =
  length xs == c ==> d == dimension (addRow m row (mod r r'))
  where d@(r',c) = dimension m

addCol :: CommutativeRing a => Matrix a -> Vector a -> Int -> Matrix a
addCol m c x = transpose $ addRow (transpose m) c x

subRow, subCol :: CommutativeRing a => Matrix a -> Vector a -> Int -> Matrix a
subRow m (Vec xs) x = addRow m (Vec (map neg xs)) x
subCol m (Vec xs) x = addCol m (Vec (map neg xs)) x

-- Multiply the pivot row and add it to the target row.
pivot :: CommutativeRing a => Matrix a -> a -> Int -> Int -> Matrix a
pivot m s p t = addRow m (fmap (s <*>) (unM m !! p)) t

-- Find first non-zero number below the pivot and return its value and row number
-- given that it exists
findPivot :: (CommutativeRing a, Eq a) => Matrix a -> (Int,Int) -> Maybe (a,Int)
findPivot m (r,c) = safeHead $ filter ((/= zero) . fst) $ drop (r+1) $ zip (head $ drop c $ unMVec $ transpose m) [0..]
  where
  m' = unMVec m

  safeHead []     = Nothing
  safeHead (x:xs) = Just x

fE :: (Field a, Eq a) => Matrix a -> Matrix a
fE (M [])         = M []
fE (M (Vec []:_)) = M []
fE m     = case L.findIndices (/= zero) (map head xs) of
  (i:is) -> case fE (cancelOut m [ (i,map head xs !! i) | i <- is ] (i,map head xs !! i)) of
    ys -> matrix (xs !! i : map (zero :) (unMVec ys))
  []     -> case fE (matrix (map tail xs)) of
    ys -> matrix (map (zero:) (unMVec ys))
  where
  cancelOut :: (Field a, Eq a) => Matrix a -> [(Int,a)] -> (Int,a) -> Matrix a
  cancelOut m [] (i,_)    = let xs = unMVec m in matrix $ map tail (L.delete (xs !! i) xs)
  cancelOut m ((t,x):xs) (i,p) = cancelOut (pivot m (neg (x </> p)) i t) xs (i,p)

  xs = unMVec m


-- | Compute row echelon form of a system Ax=b.
forwardElim :: (Field a, Eq a) => (Matrix a,Vector a) -> (Matrix a,Vector a)
forwardElim (m,v) = fE m' (0,0)
  where
  -- fE takes the matrix to eliminate and the current row and column
  fE :: (Field a, Eq a) => Matrix a -> (Int,Int) -> (Matrix a,Vector a)
  fE (M []) _  = error "forwardElim: Empty input matrix"
  fE m rc@(r,c)
      -- The algorithm is done when it reaches the last column or row.
    | c == mc || r == mr =
      -- Decompose the matrix into A and b again
      (matrix *** Vec) $ unzip $ map (init &&& last) $ unMVec m

    | m !!! rc == zero   = case findPivot m rc of
      -- If the pivot element is zero swap the pivot row with the first row
      -- with a nonzero element in the pivot column.
      Just (_,r') -> fE (swap m r r') rc
      -- If all elements in the pivot column is zero the move right.
      Nothing     -> fE m (r,c+1)

    | m !!! rc /= one    =
      -- Make the pivot element 1.
      fE (scale m r (inv (m !!! rc))) rc

    | otherwise          = case findPivot m rc of
      -- Make the first nonzero element in the pivot row 0.
      Just (v,r') -> fE (pivot m (neg v) r r') (r,c)
      -- If all elements in the pivot column is zero then move down and right.
      Nothing     -> fE m (r+1,c+1)

  (mr,mc) = dimension m

  -- Combine A and b to a matrix where the last column is b
  m' = matrix $ [ r ++ [x] | (r,x) <- zip (unMVec m) (unVec v) ]


-- | Perform "jordan"-step in Gauss-Jordan elimination. That is make every
-- element above the diagonal zero. In other words compute the reduced
-- echelon form of a matrix given that the input is in row echelon form.
jordan :: (Field a, Eq a) => (Matrix a, Vector a) -> (Matrix a, Vector a)
jordan (m, Vec ys) = case L.unzip (jordan' (zip (unMVec m) ys) (r-1)) of
  (a,b) -> (matrix a, Vec b)
  where
  (r,_) = dimension m

  jordan' [] _ = []
  jordan' xs c =
    jordan' [ (take c x ++ zero : drop (c+1) x, v <-> x !! c <*> snd (last xs))
            | (x,v) <- init xs ] (c-1) ++ [last xs]


-- | Gauss-Jordan elimination: Given A and B solve Ax=B.
gaussElim :: (Field a, Eq a, Show a) => (Matrix a, Vector a) -> (Matrix a, Vector a)
gaussElim = jordan . forwardElim

gaussElimCorrect :: (Field a, Eq a, Arbitrary a, Show a) => (Matrix a, Vector a) -> Property
gaussElimCorrect m@(a,b) = fst (dimension a) == lengthVec b && isSquareMatrix a ==>
  matrixToVector (transpose (a `mulM` transpose (M [snd (gaussElim m)]))) == b
