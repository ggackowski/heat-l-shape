import Gauss
import FElements
import FElementsDx
import FElementsDy
--import Diagram

e x y = [e1 x y, e2 x y, e3 x y, e4 x y, e5 x y]

e_dx x y = [e1_dx x y, e2_dx x y, e3_dx x y, e4_dx x y, e5_dx x y]

e_dy x y = [e1_dy x y, e2_dy x y, e3_dy x y, e4_dy x y, e5_dy x y]

avgX i = (((snd $ domainsX !! i) + (fst $ domainsX !! i)) / 2)
avgY i = (((snd $ domainsY !! i) + (fst $ domainsY !! i)) / 2)

b_side i j = 
  (e_dx (avgX i) (avgY i) !! i) * 
  (e_dx (avgX j) (avgY j) !! j) +
  (e_dy (avgX i) (avgY i) !! i) *
  (e_dy (avgX j) (avgY j) !! j)

l_side = [0.814980, 1, 0.814980, 0, 0.814980]

zeros = [(0, 2), (0, 3),(0, 4), (1, 4), (2, 4), (2, 0), (3, 0), (4, 0), (4, 1), (4, 2)]

matrix = [[if (i, j) `elem` zeros then 0 else b_side i j | i <- [0..4] ] ++ [l_side !! j] | j <- [0..4]]

result = solve $ matrix

result = [
  -4.88988,
  13.03968,
  -25.48908,
  5.03968,
  -0.88988
  ]
u x y = realToFrac $ foldr (+) 0 $ zipWith (*) result $ e x y

--main = print . solve $ [[1,0,0,0,0,1],[0,2,0,0,0,2],[0,0,4,0,0,3],[0,0,0,1,0,4],[0,0,0,0,1,5]]

--main = visualize 200 u