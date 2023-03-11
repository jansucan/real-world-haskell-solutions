-- Turn a list into a palindrome, i.e. it should read the same both backwards
-- and forwards. For example, given the list [1,2,3], your function should
-- return [1,2,3,3,2,1].

makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome (x:xs) = [x] ++ (makePalindrome xs) ++ [x]

-- ghci> :l 3_b_4.hs
-- [1 of 1] Compiling Main             ( 3_b_4.hs, interpreted )
-- Ok, one module loaded.
-- ghci> makePalindrome []
-- []
-- ghci> makePalindrome [1, 2, 3]
-- [1,2,3,3,2,1]
