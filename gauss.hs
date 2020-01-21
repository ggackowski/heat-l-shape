{-BSD 2-Clause License

Copyright (c) 2018, kikaxa
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Gauss (
  solve
)
where

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

s m = (( (*) 10 $ head $ head m) : (tail $ head m)) : tail m

substitute matrix = foldl next [] matrix where
 lengthmatrix = length matrix
 next found row = let
  subpart = init $ drop (lengthmatrix - length found) row
  solution = last row - sum (zipWith (*) found subpart)
  in solution : found

solve :: Matrix -> Row
solve = substitute . gaussianReduce_ . s

