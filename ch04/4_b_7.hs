-- Write your own definition of the standard takeWhile function, first using
-- explicit recursion, and then foldr.

myTakeWhileRecursive :: (a -> Bool) -> [a] -> [a]
myTakeWhileRecursive f (x:xs) = if f x
                                then x:(myTakeWhileRecursive f xs)
                                else []
myTakeWhileRecursive _ [] = []


myTakeWhileFoldr :: (a -> Bool) -> [a] -> [a]
myTakeWhileFoldr f xs = foldr op [] xs
  where op x acc = if f x
                   then x:acc
                   else [] -- Folding from the right. When function f is false for x,
                           -- the accumulated list is thrown away, thus eventually
                           -- keeping only the leftmost sublist for which the f is
                           -- true for all its items.

-- ghci> :l 4_b_7.hs
-- [1 of 1] Compiling Main             ( 4_b_7.hs, interpreted )
-- Ok, one module loaded.
-- ghci> takeWhile odd []
-- []
-- ghci> takeWhile odd [2, 1]
-- []
-- ghci> takeWhile odd [1, 3, 2, 7]
-- [1,3]
-- ghci> myTakeWhileRecursive odd []
-- []
-- ghci> myTakeWhileRecursive odd [2, 1]
-- []
-- ghci> myTakeWhileRecursive odd [1, 3, 2, 7]
-- [1,3]
-- ghci> myTakeWhileFoldr odd []
-- []
-- ghci> myTakeWhileFoldr odd [2, 1]
-- []
-- ghci> myTakeWhileFoldr odd [1, 3, 2, 7]
-- [1,3]
