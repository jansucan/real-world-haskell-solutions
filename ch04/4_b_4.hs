-- The asInt_fold function uses error, so its callers cannot handle
-- errors. Rewrite it to fix this problem:
--
--   -- file: ch04/ch04.exercises.hs
--   type ErrorMessage = String
--   asInt_either :: String -> Either String Int
--
--   ghci> asInt_either "33"
--   Right 33
--   ghci> asInt_either "foo"
--   Left "non-digit 'o'"

-- This exercise is based on the previous one (4_b_3.hs). There was only one
-- call to error, so only that call is replaced by Either in this solution.

-- The example for argument "foo" in the assignment returns 'o' as the first
-- non-digit character. This seems incorrect as the 'f' is the first
-- non-digit. This implementation checks every character other than the minus
-- sign when checking for non-digits.

import Data.Char (digitToInt, isDigit)

asInt_either :: String -> Either String Int
asInt_either "" = Right 0
asInt_either "-" = Right 0
asInt_either ('-':xs) = propagateEither int
  where int = asInt_either' xs
        propagateEither (Right x) = Right (-1 * x)
        propagateEither left = left
asInt_either xs = asInt_either' xs

asInt_either' :: String -> Either String Int
asInt_either' xs = foldl nextDigit (Right 0) xs
  where nextDigit (Right acc) digit
          | not (isDigit digit) = Left ("non-digit '" ++ [digit] ++ "'")
          | otherwise = Right ((acc * 10) + (digitToInt digit))
        nextDigit left _ = left

-- ghci> :l 4_b_4.hs
-- [1 of 1] Compiling Main             ( 4_b_4.hs, interpreted )
-- Ok, one module loaded.
-- ghci> asInt_either "33"
-- Right 33
-- ghci> asInt_either "foo"
-- Left "non-digit 'f'"
-- ghci> asInt_either "-33"
-- Right (-33)
