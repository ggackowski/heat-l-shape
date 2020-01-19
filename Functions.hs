module Functions 
( f1
, f2
, f3
, f4
, f1_dx
, f1_dy
, f2_dx
, f2_dy
, f3_dx
, f3_dy
, f4_dx
, f4_dy
) where

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
