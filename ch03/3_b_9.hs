-- 1. Consider three two-dimensional points a, b, and c. If we look at the angle
--    formed by the line segment from a to b and the line segment from b to c,
--    it either turns left, turns right, or forms a straight line. Define a
--    Direction data type that lets you represent these possibilities.
--
-- 2. Write a function that calculates the turn made by three 2D points and
--    returns a Direction.
--
-- 3. Define a function that takes a list of 2D points and computes the
--    direction of each successive triple. Given a list of points [a,b,c,d,e],
--    it should begin by computing the turn made by [a,b,c], then the turn made
--    by [b,c,d], then [c,d,e]. Your function should return a list of Direction.

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

listDirections :: [Point] -> [Direction]
listDirections (a:b:c:xs) = (direction a b c):(listDirections ([b,c] ++ xs))
listDirections _ = []

-- ghci> :l 3_b_9.hs
-- [1 of 1] Compiling Main             ( 3_b_9.hs, interpreted )
-- Ok, one module loaded.
-- ghci> listDirections [(1,1), (2,2), (3,3), (4,5), (6,1)]
-- [DStraight,DLeft,DRight]
