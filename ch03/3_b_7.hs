-- Define a function that joins a list of lists together using a separator value.
--
--     -- file: ch03/Intersperse.hs
--     intersperse :: a -> [[a]] -> [a]
--
-- The separator should appear between elements of the list, but should not
-- follow the last element. Your function should behave as follows.
--
--     ghci> :load Intersperse
--     [1 of 1] Compiling Main             ( Intersperse.hs, interpreted )
--     Ok, modules loaded: Main.
--     ghci> intersperse ',' []
--     ""
--     ghci> intersperse ',' ["foo"]
--     "foo"
--     ghci> intersperse ',' ["foo","bar","baz","quux"]
--     "foo,bar,baz,quux"

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ (x:[]) = x
myIntersperse s (x:xs) = x ++ s:(myIntersperse s xs)

-- ghci> :l 3_b_7.hs
-- [1 of 1] Compiling Main             ( 3_b_7.hs, interpreted )
-- Ok, one module loaded.
-- ghci> myIntersperse ',' []
-- ""
-- ghci> myIntersperse ',' ["foo"]
-- "foo"
-- ghci> myIntersperse ',' ["foo","bar","baz","quux"]
-- "foo,bar,baz,quux"
