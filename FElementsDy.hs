module FElementsDy (
    e1_dy,
    e2_dy,
    e3_dy,
    e4_dy,
    e5_dy
)
where

import Functions

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
