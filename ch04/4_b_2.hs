-- 1. Use a fold (choosing the appropriate fold will make your code much
--    simpler) to rewrite and improve upon the asInt function from the earlier
--    section called "Explicit recursion" on page 85.
--
--      -- file: ch04/ch04.exercises.hs
--      asInt_fold :: String -> Int
--
-- 2. Your function should behave as follows:
--
--      ghci> asInt_fold "101"
--      101
--      ghci> asInt_fold "-31337"
--      -31337
--      ghci> asInt_fold "1798"
--      1798

import Data.Char (digitToInt)

-- If we used foldr, it would have needed to reconstruct position of the digit
-- processed in the step function (here nextDigit). When digits are processed
-- from right to left, we need to know position of the digit in the number so we
-- can multiply it by the corresponding power of 10 before adding it to the
-- accumulator. The information about the position would have to be gotten from
-- a value already in the accumulator.
--
-- With foldl, when digits are processed from left to right, the information
-- about the position is not needed. We just multiply the accumulator by ten to
-- shift the decimal value to the left, thus making room for one more digit, and
-- then add value of that digit to the accumulator.

asInt_fold :: String -> Int
asInt_fold ('-':xs) = -1 * (asInt_fold' xs)
asInt_fold xs = asInt_fold' xs

asInt_fold' :: String -> Int
asInt_fold' xs = foldl nextDigit 0 xs
  where nextDigit acc digit = (acc * 10) + (digitToInt digit)

-- ghci> :l 4_b_2.hs
-- [1 of 1] Compiling Main             ( 4_b_2.hs, interpreted )
-- Ok, one module loaded.
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798
