-- Write a function that computes the number of elements in a list. To test it,
-- ensure that it gives the same answers as the standard length function.

myLength (x:xs) = 1 + myLength xs
myLength []     = 0

-- ghci> :l 3_b_1.hs
-- [1 of 1] Compiling Main             ( 3_b_1.hs, interpreted )
-- Ok, one module loaded.
-- ghci> myLength []
-- 0
-- ghci> myLength [1]
-- 1
-- ghci> myLength [1, 2]
-- 2
-- ghci> myLength [1, 2, 3]
-- 3
-- ghci> length []
-- 0
-- ghci> length [1]
-- 1
-- ghci> length [1, 2]
-- 2
-- ghci> length [1, 2, 3]
-- 3
