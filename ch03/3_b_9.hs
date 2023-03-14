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
--
-- 4. Using the code from the preceding three exercises, implement Graham's scan
--    algorithm for the convex hull of a set of 2D points. You can find good
--    description of what a convex hull
--    (http://en.wikipedia.org/wiki/Convex_hull) is, and how the Graham scan
--    algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work, on
--    Wikipedia (http://en.wikipedia.org/).

import Data.List

data Direction = DLeft
               | DRight
               | DStraight
               deriving (Show, Eq)

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

-- -----------------------------------------------------------------------------

-- Test input points:
--
-- 6 |     x
-- 5 | x
-- 4 x    xx
-- 3 | xx  x
-- 2 x x
-- 1 |x   x x
-- 0 +-------
--   01234567

inputData :: [Point]
inputData = [(0, 2), (0, 4), (1, 1), (2, 2), (2, 3), (2, 5), (3, 3), (5, 1),
             (5, 4), (6, 3), (6, 4), (6, 6), (7, 1)]

-- Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan):
--
-- The first step in this algorithm is to find the point with the lowest
-- y-coordinate. If the lowest y-coordinate exists in more than one point in the
-- set, the point with the lowest x-coordinate out of the candidates should be
-- chosen. Call this point P.

basePointCompare :: Point -> Point -> Ordering
basePointCompare (x1, y1) (x2, y2)
  | y1 < y2 = LT
  | y1 > y2 = GT
  | x1 < x2 = LT
  | x1 > x2 = GT
  | otherwise = EQ

basePoint = head (Data.List.sortBy basePointCompare inputData)

-- Continuation of Graham scan algorithm
-- (http://en.wikipedia.org/wiki/Graham_scan):
--
-- Next, the set of points must be sorted in increasing order of the angle they
-- and the point P make with the x-axis.
--
-- Sorting in order of angle does not require computing the angle. It is
-- possible to use any function of the angle which is monotonic in the interval
-- [0 , pi]. The cosine is easily computed using the dot product ...
--
-- If several points are of the same angle, ... delete all but the furthest
-- point.

magnitude :: Point -> Point -> Double
magnitude (x1, y1) (x2, y2) = sqrt (a^2 + b^2)
  where a = fromIntegral (abs (x1 - x2))
        b = fromIntegral (abs (y1 - y2))

-- The X axis is represented by vector (1, 0).
--
-- Instead of an angle in degrees only cosine is returned. This is sufficient
-- for sorting as suggested in the description of the algorithm.
--
-- In the interval [0 , pi], the greater the angle is, the lower the cosine
-- is. For sorting from the lowest to the greatest angle the cosine value is
-- negated.
angleWithBase :: Point -> Double
angleWithBase (x2, y2) = -cosine
  where (x1, y1) = basePoint
        x = fromIntegral (x2 - x1)
        y = fromIntegral (y2 - y1)
        dot_product = fromIntegral ((x * 1) + (y * 0))
        magnitude_a = magnitude (0, 0) (x, y)
        magnitude_b = 1
        cosine = dot_product / (magnitude_a * magnitude_b)

anglesWithXAxis :: [Point] -> [Double]
anglesWithXAxis (x:xs) = angle:(anglesWithXAxis xs)
  where angle = angleWithBase x
anglesWithXAxis [] = []

distancesFromBase :: [Point] -> [Double]
distancesFromBase (x:xs) = distance:(distancesFromBase xs)
  where distance = magnitude basePoint x
distancesFromBase [] = []

angleComparePacked :: (Point, Double, Double) -> (Point, Double, Double) -> Ordering
angleComparePacked (_, a1, d1) (_, a2, d2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | d1 < d2 = LT
  | d1 > d2 = GT
  | otherwise = EQ

filterOnlyFurthest :: [(Point, Double, Double)] -> [(Point, Double, Double)]
filterOnlyFurthest (a:b:xs) = if remove
                              then filterOnlyFurthest (b:xs)
                              else a:(filterOnlyFurthest (b:xs))
  where (_, angle_a, distance_a) = a
        (_, angle_b, distance_b) = b
        remove = (angle_a == angle_b) && (distance_a <= distance_b)
filterOnlyFurthest x = x

sorted = sortedonlyFurthest
  where angles = anglesWithXAxis inputData
        distances = distancesFromBase inputData
        packed = zip3 inputData angles distances
        sortedByAngle = Data.List.sortBy angleComparePacked packed
        onlyFurthest = filterOnlyFurthest sortedByAngle
        (sortedonlyFurthest, _, _) = unzip3 onlyFurthest

-- Continuation of Graham scan algorithm
-- (http://en.wikipedia.org/wiki/Graham_scan):
--
-- The algorithm proceeds by considering each of the points in the sorted array
-- in sequence. For each point, it is first determined whether traveling from
-- the two points immediately preceding this point constitutes making a left
-- turn or a right turn. If a right turn, the second-to-last point is not part
-- of the convex hull, and lies 'inside' it. The same determination is then made
-- for the set of the latest point and the two points that immediately precede
-- the point found to have been inside the hull, and is repeated until a "left
-- turn" set is encountered, at which point the algorithm moves on to the next
-- point in the set of points in the sorted array minus any points that were
-- found to be inside the hull; there is no need to consider these points
-- again. (If at any stage the three points are collinear, one may opt either to
-- discard or to report it, since in some applications it is required to find
-- all points on the boundary of the convex hull.)
--
-- This process will eventually return to the point at which it started, at
-- which point the algorithm is completed and the stack now contains the points
-- on the convex hull in counterclockwise order.


-- The algorithm from Wikipedia is described in a more procedural way. In order
-- to follow the assignment of this exercises and use the listDirections
-- function we need to view the algorithm in a more functional way. It means
-- processing the list of points (computing the directions) as a whole list, not
-- as individual points.
--
-- We could think of removing all points that are middle ones in all right turns
-- from the list at once, but this approach could lead to wrong results. See the
-- following example:
--
--      Points     [(0,0), (5,1), (4,1), (3,2),  (0,4)]
--      Directions [              DLeft, DRight, DLeft]
--
--   If we remove point (4,1) as a middle one in a right turn, we could wrongly
--   conclude that it is the result because there are only left turns left. But
--   when the directions are updated, we get
--
--      Points     [(0,0), (5,1), (3,2),  (0,4) ]
--      Directions [              DLeft,  DRight]
--
--   which indicates that there is some more work to do by removing point (3,2).
--
-- Updating the directions needs to be repeated until it results in no change.

filterFirstRight :: [(Point, Direction)] -> [(Point, Direction)]
filterFirstRight (a:b:c:xs) = if direction == DRight
                              then [a,c] ++ xs
                              else a:(filterFirstRight ([b,c] ++ xs))
  where direction = snd c
filterFirstRight xs = xs

  -- The first two points are paired with dummy directions. They are not used.
convexHull :: [Point] -> [Point]
convexHull points = if changed
                    then convexHull filtered
                    else points
  where directions = [DStraight, DStraight] ++ (listDirections points)
        pointsDirections = zip points directions
        filtered = fst (unzip (filterFirstRight pointsDirections))
        changed = filtered /= points

hull = convexHull sorted

-- Convex hull points found for the test input:
--
-- 6 |     x
-- 5 | x
-- 4 x    ..
-- 3 | ..  .
-- 2 x .
-- 1 |x   . x
-- 0 +-------
--   01234567
