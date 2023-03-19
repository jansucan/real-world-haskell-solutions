-- Extend your function to handle the following kinds of exceptional conditions
-- by calling error.
--
--   ghci> asInt_fold ""
--   0
--   ghci> asInt_fold "-"
--   0
--   ghci> asInt_fold "-3"
--   -3
--   ghci> asInt_fold "2.7"
--   *** Exception: Char.digitToInt: not a digit '.'
--   ghci> asInt_fold "314159265358979323846"
--   564616105916946374

-- The assignment is ambiguous. It tells us we should call error, but the
-- examples show only one example of an error situation (for input "2.7"). This
-- error is already caught and reported by the digitToInt function, producing
-- the same error as shown in the examples, so no work from us is needed to make
-- the function to behave as required.
--
-- However, the next exercise replaces the call to error by returning Either.
--
-- Let's make the function to behave as specified in assignment of this
-- exercise, calling error producing the same error output as shown in the next
-- exercise, thus preparing the code for that exercise.
--
--   ghci> asInt_fold "2.7"
--   *** Exception: non-digit '.'

import Data.Char (digitToInt, isDigit)

asInt_fold :: String -> Int
asInt_fold "" = 0
asInt_fold "-" = 0
asInt_fold ('-':xs) = -1 * (asInt_fold' xs)
asInt_fold xs = asInt_fold' xs

asInt_fold' :: String -> Int
asInt_fold' xs = foldl nextDigit 0 xs
  where nextDigit acc digit
          | not (isDigit digit) = error ("non-digit '" ++ [digit] ++ "'")
          | otherwise = (acc * 10) + (digitToInt digit)

-- ghci> :l 4_b_3.hs
-- [1 of 1] Compiling Main             ( 4_b_3.hs, interpreted )
-- Ok, one module loaded.
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: non-digit '.'
-- CallStack (from HasCallStack):
--   error, called at 4_b_3.hs:41:35 in main:Main
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374
