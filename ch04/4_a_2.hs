-- Write a function splitWith that acts similarly to words, but takes a
-- predicate and a list of any type, and splits its input list on every element
-- for which the predicate returns False.
--
--   -- file: ch04/ch04.exercises.hs
--   splitWith :: (a -> Bool) -> [a] -> [[a]]

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f xs = if not (null first)
                 then first:splitRest
                 else splitRest
  where (first, suffix) = span f xs
        rest = safeTail suffix
        splitRest = splitWith f rest

-- ghci> :l 4_a_2.hs
-- [1 of 1] Compiling Main             ( 4_a_2.hs, interpreted )
-- Ok, one module loaded.
-- ghci> splitWith even [2, 1, 4, 4, 3, 3, 6, 6, 6]
-- [[2],[4,4],[6,6,6]]
