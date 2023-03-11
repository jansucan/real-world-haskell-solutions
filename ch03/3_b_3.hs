-- Write a function that computes the mean of a list, i.e. the sum of all
-- elements in the list divided by its length. (You may need to use the
-- fromIntegral function to convert the length of the list from an integer into
-- a floating point number.)

sumOfList [] = 0
sumOfList (x:xs) = x + (sumOfList xs)

meanOfList [] = 0
meanOfList xs = sum / (fromIntegral len)
  where sum = sumOfList xs
        len = length xs

-- ghci> :l 3_b_3.hs
-- [1 of 1] Compiling Main             ( 3_b_3.hs, interpreted )
-- Ok, one module loaded.
-- ghci> meanOfList []
-- 0.0
-- ghci> meanOfList [1]
-- 1.0
-- ghci> meanOfList [1, 3]
-- 2.0
-- ghci> meanOfList [2.3, 4.5, 6.9]
-- 4.566666666666666
