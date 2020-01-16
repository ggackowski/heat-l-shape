
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
 
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour (withOpacity)





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

func x y = 3 * (e x y !! 0) + 1 * (e x y !! 1) + 0.3 *  (e x y !! 2) + 0.2 *  (e x y !! 3) + 1.3 *  (e x y !! 4)






--wyswietlana funkcja
--func :: Double -> Double -> Double
--func x y =  if x >= 0 && y >= 0 then -2 else sin (3*x) + cos (3*y)


--kolorowanie
cell :: Double -> Diagram B 
cell value = square 1 # fcA (color `withOpacity` value) # lw none
 where 
  color
    | value > 0.8 = darkred
    | value > 0.6 = red
    | value > 0.5 = orange
    | value > 0.4 = yellow 
    | value > 0.2 = blue
    | otherwise = darkblue


--szukanie min max wartosci
sort :: [Double] -> [Double]
sort [] = []
sort (x:xs) = sort less ++ exs ++ sort greater
 where less = filter (\k -> k < x) (x:xs)
       greater = filter (\k -> k > x) (x:xs)
       exs = filter (\k -> k == x) (x:xs)
minv :: [Double] -> Double
minv x = head $ sort x
maxv :: [Double] -> Double
maxv x = head $ reverse $ sort x 


--szerokosc kratki
gridStep domX domY gsize = ((snd domX) - (fst domX)) / (fromIntegral gsize)

--wartosc funkcji w danej kratce
functionAtT f n m domX domY gstep = f (min (fst domX) (snd domX) + n * gstep) ( max (fst domY) (snd domY) - m * gstep)
--normalizacja do (0, 1)
normalizest x minValue maxValue = (x - minValue) / (maxValue - minValue)


makeGrid gridS dX dY fun = hcat (map vcat cells)
 where 
  gst = gridStep dX dY gridS
  functionAt f n m = functionAtT f n m dX dY gst
  allV = [functionAt fun (fromIntegral x) (fromIntegral y) | x <- [1 .. gridS], y <- [1 .. gridS]]
  minValue = minv allV
  maxValue = maxv allV
  normalizes x = normalizest x minValue maxValue
  cells = [ [cell $ normalizes $ functionAt fun (fromIntegral j) (fromIntegral i) | i <- [1..gridS] ] | j <- [1..gridS] ]

gridTmp = makeGrid 40 (-1, 1) (-1, 1) func 

main = mainWith $ gridTmp



  