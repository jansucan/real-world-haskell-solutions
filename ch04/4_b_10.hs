-- How many of the following Prelude functions can you rewrite using list folds?
--
--  - any
--  - cycle
--  - words
--  - unlines
--
-- For those functions where you can use either foldl' or foldr, which is more
-- appropriate in each case?

-- Let's assume that rewriting here means using a fold function as the top-level
-- function, using its output directly as an output of a rewritten function, and
-- not using any recursion in the fold's step function.
--
-- "Appropriate" is a vague term. It can mean appropriate with regards to
--   - processing an infinite input list, or
--   - lazy evaluation in the case of a finite input list, not evaluating
--     subexpressions when not necessary, or
--   - learning/teaching purpose, or
--   - natural way of thinking about the problem to solve (e.g, people
--     usually read books from the first page to the last one, not the other way
--     around), or
--   - ...
--
-- For deciding between foldl' and foldr, let's assume only finite input lists.

-- References:
--
-- - This article helped me to understand how lazy evaluation works with foldr
--   https://elbauldelprogramador.com/org-posts/foldr-infinite-list-haskell.html
-- - Description of the words function
--   https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#v:words
-- - Description of the unlines function
--   https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#v:unlines

import Data.Char

-- Let's use foldr. It enables short-circuit evaluation, not continuing in
-- evaluation of the rest of the input if the first value for which the f
-- function is true is found.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr op False xs
  where op x acc = if f x
                   then True
                   else acc

-- ghci> :l 4_b_10.hs
-- [1 of 1] Compiling Main             ( 4_b_10.hs, interpreted )
-- Ok, one module loaded.
-- ghci> any odd []
-- False
-- ghci> any odd [2, 4, 6, 8]
-- False
-- ghci> any odd [2, 4, 7, 8]
-- True
-- ghci> myAny odd []
-- False
-- ghci> myAny odd [2, 4, 6, 8]
-- False
-- ghci> myAny odd [2, 4, 7, 8]
-- True


-- The cycle function cannot be rewritten using the list folds. It outputs an
-- infinite list. The input is a finite list. For a finite list, the step
-- function is called finite number of times, so without using recursion, it is
-- not possible to generate an infinite output.


-- The words function cannot be rewritten using the list folds. For example, the
-- function must be able to transform " foo bar " into ["foo", "bar"]. With
-- using fold as we defined it for this exercise, it is not possible to handle
-- trailing (or leading; depending on whether we use foldl' or foldr) spaces. It
-- would be possible if we allowed post-processing the output of the fold,
-- trimming the trailing empty string.
--
-- Without post-processing, we have to find a way of not adding a trailing empty
-- string into the output for the trailing spaces in the input. This could be
-- done either by adding the empty string only after encountering a non-space
-- character (and putting that character into that empty string), or by adding
-- it when encountering a space character (correctly handling multiple
-- successive spaces).
--
-- With the first option, we would have to ignore input spaces because we expect
-- a new empty string to be added to the output on a non-space input character,
-- but this would result in putting every non-space character into its own
-- output list.
--
-- With the second option, we would add an empty list into the output for the
-- last trailing space, but it would stay empty in the output when there is no
-- non-space character to be put into that list.


-- Let's use foldr to take advantage of lazy evaluation.
myUnlines :: [String] -> String
myUnlines xs = foldr op [] xs
  where op x acc = x ++ "\n" ++ acc

-- ghci> unlines []
-- ""
-- ghci> unlines ["abc","","","def "]
-- "abc\n\n\ndef \n"
-- ghci> myUnlines []
-- ""
-- ghci> myUnlines ["abc","","","def "]
-- "abc\n\n\ndef \n"
