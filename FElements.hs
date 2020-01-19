module FElements (
    e1,
    e2,
    e3,
    e4,
    e5,
    domainsX,
    domainsY
)
where

import Functions


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
      | x > -1 && x <= 0 && y >= -1 && y <= 0 = f2 (x + 1) (y + 1)
      | otherwise = 0

e5 x y 
      | x >= 0 && x <= 1 && y >= -1 && y <= 0 = f2 x (y + 1)
      | otherwise = 0

domainsX :: [(Float, Float)]
domainsX = [(-1, 0), (-1, 0), (-1, 0), (-1, 1), (0, 1)]
domainsY :: [(Float, Float)]
domainsY = [(0, 1), (-1, 1), (-1, 0), (-1, 0), (-1, 0)]