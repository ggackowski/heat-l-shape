import Control.DeepSeq

type Row = [Float]
type Matrix = [Row]

genRow z n = foldl (\y x -> 1 / (z + x) : y) [1 / (z+1)] [n,n-1..1]
genMatr n = map (\x -> genRow x n) [1..n]

gaussianReduce = reverse . gaussianReduce_
gaussianReduce_ :: Matrix -> Matrix
gaussianReduce_ matrix = fst $ foldl reduceRow ([] :: Matrix, matrix) [0..length matrix-1] where

 swap xs a b
  | a > b = swap xs b a
  | a == b = xs
  | a < b = let
  (p1,p2) = splitAt a xs
  (p3,p4) = splitAt (b-a-1) (tail p2)
  in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)

 reduceRow (rst, matrix1) r = let
  matrix2 = if (matrix1 !! 0 !! r == 0) then (swap matrix1 0 firstnonzero) else matrix1
    where firstnonzero = head $ filter (\x -> matrix1 !! 0 !! r /= 0) [0..length matrix1-1]
  row1 = map (\x -> x / (row !! r)) row
    where row = matrix2 !! 0

  subrow nr = zipWith (\a b -> k*a - b) row1 nr
    where k = nr !! r
  nextrows = map subrow $ drop 1 matrix2
  in (row1 : rst, force $ nextrows)

substitute matrix = foldl next [] matrix where
 lengthmatrix = length matrix
 next found row = let
  subpart = init $ drop (lengthmatrix - length found) row
  solution = last row - sum (zipWith (*) found subpart)
  in solution : found


solve :: Matrix -> Row
solve = substitute . gaussianReduce_

main = print . solve $ [[2, -1, 3, 5], [2, 2, 3, 7], [-2, 3, 0, -3]]