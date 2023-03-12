-- 1. Consider three two-dimensional points a, b, and c. If we look at the angle
--    formed by the line segment from a to b and the line segment from b to c,
--    it either turns left, turns right, or forms a straight line. Define a
--    Direction data type that lets you represent these possibilities.
--
-- 2. Write a function that calculates the turn made by three 2D points and
--    returns a Direction.

data Direction = DLeft
               | DRight
               | DStraight
               deriving (Show)

type Point = (Int, Int)

-- The algorithm for computing the direction is taken from
-- https://en.wikipedia.org/wiki/Graham_scan
direction :: Point -> Point -> Point -> Direction
direction (x1, y1) (x2, y2) (x3, y3)
  | cross_product_z > 0 = DLeft
  | cross_product_z < 0 = DRight
  | otherwise = DStraight
  where cross_product_z = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

-- [1 of 1] Compiling Main             ( 3_b_9.hs, interpreted )
-- Ok, one module loaded.
-- ghci> direction (1,1) (2,2) (3,3)
-- DStraight
-- ghci> direction (1,1) (2,2) (3,4)
-- DLeft
-- ghci> direction (1,1) (2,2) (3,2)
-- DRight
