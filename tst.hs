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



f1 x y = (1 - x) * (1 - y)
f2 x y = (1 - y) * x 
f3 x y = x * y
f4 x y = (1 - x) * y 

f1_dx x y = (- (1 - y))
f1_dy x y = (-(1 - x))

f2_dx x y = 1 - y
f2_dy x y = (-x)

f3_dx x y = y
f3_dy x y = x

f4_dx x y = (-y)
f4_dy x y = 1 - x


e1 x y 
      | x >= -1 && x <= 0 && y >= 0 && y <= 1 = f4 (x + 1) y 
      | otherwise = 0

domainsX :: [(Float, Float)]
domainsX = [(-1, 0), (-1, 0), (-1, 0), (-1, 1), (0, 1)]
domainsY :: [(Float, Float)]
domainsY = [(0, 1), (-1, 1), (-1, 0), (-1, 0), (-1, 0)]

e2 x y
      | x >= -1 && x <= 0 && y >= 0 && y <= 1 = f1 (x + 1) y 
      | x >= -1 && x <= 0 && y >= -1 && y < 0 = f4 (x + 1) (y + 1)
      | otherwise = 0

e3 x y 
      | x >= -1 && x <= 0 && y >= -1 && y <= 0 = f1 (x + 1) (y + 1)
      | otherwise = 0

e4 x y 
      | x >= 0 && x <= 1 && y >= -1 && y <= 0 = f1 x (y + 1)
      | x > -1 && x <= 0 && y >= -1 && y <= 0 = f2 x (y + 1)
      | otherwise = 0

e5 x y 
      | x >= 0 && x <= 1 && y >= -1 && y <= 0 = f2 x (y + 1)
      | otherwise = 0


e1_dx x y 
      | x >= -1 && x <= 0 && y >= 0 && y <= 1 = f4_dx (x + 1) y 
      | otherwise = 0

e2_dx x y
    | x >= -1 && x <= 0 && y >= 0 && y <= 1 = f1_dx (x + 1) y 
    | x >= -1 && x <= 0 && y >= -1 && y < 0 = f4_dx (x + 1) (y + 1)
    | otherwise = 0

e3_dx x y 
    | x >= -1 && x <= 0 && y >= -1 && y <= 0 = f1_dx (x + 1) (y + 1)
    | otherwise = 0

e4_dx x y 
    | x >= 0 && x <= 1 && y >= -1 && y <= 0 = f1_dx x (y + 1)
    | x > -1 && x <= 0 && y >= -1 && y <= 0 = f2_dx x (y + 1)
    | otherwise = 0

e5_dx x y 
    | x >= 0 && x <= 1 && y >= -1 && y <= 0 = f2_dx x (y + 1)
    | otherwise = 0


e1_dy x y 
    | x >= -1 && x <= 0 && y >= 0 && y <= 1 = f4_dy (x + 1) y 
    | otherwise = 0

e2_dy x y
    | x >= -1 && x <= 0 && y >= 0 && y <= 1 = f1_dy (x + 1) y 
    | x >= -1 && x <= 0 && y >= -1 && y < 0 = f4_dy (x + 1) (y + 1)
    | otherwise = 0

e3_dy x y 
    | x >= -1 && x <= 0 && y >= -1 && y <= 0 = f1_dy (x + 1) (y + 1)
    | otherwise = 0

e4_dy x y 
    | x >= 0 && x <= 1 && y >= -1 && y <= 0 = f1_dy x (y + 1)
    | x > -1 && x <= 0 && y >= -1 && y <= 0 = f2_dy x (y + 1)
    | otherwise = 0

e5_dy x y 
    | x >= 0 && x <= 1 && y >= -1 && y <= 0 = f2_dy x (y + 1)
    | otherwise = 0

e x y = [ee1, ee2, ee3, ee4, ee5]
  where 
    ee1 = e1 x y
    ee2 = e2 x y
    ee3 = e3 x y
    ee4 = e4 x y
    ee5 = e5 x y

e_dx x y = [ee1, ee2, ee3, ee4, ee5]
  where 
    ee1 = e1_dx x y
    ee2 = e2_dx x y
    ee3 = e3_dx x y
    ee4 = e4_dx x y
    ee5 = e5_dx x y

e_dy x y = [ee1, ee2, ee3, ee4, ee5]
  where 
    ee1 = e1_dy x y
    ee2 = e2_dy x y
    ee3 = e3_dy x y
    ee4 = e4_dy x y
    ee5 = e5_dy x y

f x y = 3 * (e x y !! 0) + 1 * (e x y !! 1) + 0.3 *  (e x y !! 2) + 0.2 *  (e x y !! 3) + 1.3 *  (e x y !! 4)

b_side i j = 
  (e_dx (((snd $ domainsX !! i) + (fst $ domainsX !! i)) / 2) (((snd $ domainsY !! i) + (fst $ domainsY !! i)) / 2) !! i) * 
  (e_dx (((snd $ domainsX !! j) + (fst $ domainsX !! j)) / 2) (((snd $ domainsY !! j) + (fst $ domainsY !! j)) / 2) !! j) +
  (e_dy (((snd $ domainsX !! i) + (fst $ domainsX !! i)) / 2) (((snd $ domainsY !! i) + (fst $ domainsY !! i)) / 2) !! i) *
  (e_dy (((snd $ domainsX !! j) + (fst $ domainsX !! j)) / 2) (((snd $ domainsY !! j) + (fst $ domainsY !! i)) / 2) !! j)



l_side j = let k = if val < 0 then (-((-val) ** (2/3))) else (val) ** (2/3) in (1/6) * (k * (e (((snd $ domainsX !! j) + (fst $ domainsX !! j)) / 3) (-1) !! j) )
    where val = (((snd $ domainsX !! j) + (fst $ domainsX !! j)) / 3)

matrix = [[b_side i j | i <- [0..4] ] ++ [l_side j] | j <- [0..4]]

main = print . solve $ [[2, -1, 3, 5], [2, 21111, 31, 7], [-2, 3, 11, -3]]