-- 1. The Prelude function concat concatenates a list of lists into a single list
--    and has the following type:
--
--      -- file: ch04/ch04.exercises.hs
--      concat :: [[a]] -> [a]
--
-- 2. Write your own definition of concat using foldr.

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

-- ghci> :l 4_b_6.hs
-- [1 of 1] Compiling Main             ( 4_b_6.hs, interpreted )
-- Ok, one module loaded.
-- ghci> concat []
-- []
-- ghci> concat [[]]
-- []
-- ghci> concat [[1]]
-- [1]
-- ghci> concat [[1], [2,3], [4,5,6]]
-- [1,2,3,4,5,6]
-- ghci> myConcat []
-- []
-- ghci> myConcat [[]]
-- []
-- ghci> myConcat [[1]]
-- [1]
-- ghci> myConcat [[1], [2,3], [4,5,6]]
-- [1,2,3,4,5,6]
