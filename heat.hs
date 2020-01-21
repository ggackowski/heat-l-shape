-- usage: 
-- 1) cabal sandbox init
-- 2) cabal install diagrams
-- 3) cabal exec -- ghc --make heat.hs && ./heat.exe -o heat.svg -w SIZE
--    where SIZE = width of diagram
--    then open heat.svg

import Gauss
import FElements
import FElementsDx
import FElementsDy
import Diagram

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

l_side = [-0.314980, -0.845683, -1, 0, 0.614980]

zeros = [(0, 2), (0, 3),(0, 4), (1, 4), (2, 4), (2, 0), (3, 0), (4, 0), (4, 1), (4, 2)]

matrix = [[if (i, j) `elem` zeros then 0 else b_side i j | i <- [0..4] ] ++ [l_side !! j] | j <- [0..4]]

result = solve $ matrix

u x y = realToFrac $ foldr (+) 0 $ zipWith (*) result $ e x y

main = visualize 200 u