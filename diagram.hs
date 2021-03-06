module Diagram (
    visualize
)
where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
 
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour (withOpacity)

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

--tworzenie siatki
makeGrid gridS dX dY fun = hcat (map vcat cells)
 where 
  gst = gridStep dX dY gridS
  functionAt f n m = functionAtT f n m dX dY gst
  allV = [functionAt fun (fromIntegral x) (fromIntegral y) | x <- [1 .. gridS], y <- [1 .. gridS]]
  minValue = minv allV
  maxValue = maxv allV
  fun' x y = if x > 0 && y > 0 then minValue else fun x y
  normalizes x = normalizest x minValue maxValue
  cells = [ [cell $ normalizes $ functionAt fun' (fromIntegral j) (fromIntegral i) | i <- [1..gridS] ] | j <- [1..gridS] ]

--wizualizacja
visualize size function = mainWith $ makeGrid size (-1, 1) (-1, 1) function