module FElementsDx (
    e1_dx,
    e2_dx,
    e3_dx,
    e4_dx,
    e5_dx
)
where

import Functions

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