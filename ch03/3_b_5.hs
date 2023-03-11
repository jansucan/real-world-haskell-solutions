-- Write a function that determines whether its input list is a palindrome.

reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

isPalindrome xs = xs == (reverseList xs)

-- ghci> :l 3_b_5.hs
-- [1 of 1] Compiling Main             ( 3_b_5.hs, interpreted )
-- Ok, one module loaded.
-- ghci> isPalindrome []
-- True
-- ghci> isPalindrome [1]
-- True
-- ghci> isPalindrome [1, 2]
-- False
-- ghci> isPalindrome [1, 2, 1]
-- True
-- ghci> isPalindrome [1, 2, 2, 1]
-- True
-- ghci> isPalindrome [1, 2, 3, 2, 4]
-- False
