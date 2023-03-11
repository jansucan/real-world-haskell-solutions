-- Create a function that sorts a list of lists based on the length of each
-- sublist. (You may want to look at the sortBy function from the Data.List
-- module.)

import Data.List

myCompare :: [a] -> [a] -> Ordering
myCompare a b = if len_a < len_b
                then LT
                else if len_a == len_b
                     then EQ
                     else GT
  where len_a = length a
        len_b = length b

mySort :: [[a]] -> [[a]]
mySort xs = Data.List.sortBy myCompare xs

-- ghci> :l 3_b_6.hs
-- [1 of 1] Compiling Main             ( 3_b_6.hs, interpreted )
-- Ok, one module loaded.
-- ghci> mySort []
-- []
-- ghci> mySort [[2, 3], [7, 8, 9, 10], [1], [], [4, 5, 6]]
-- [[],[1],[2,3],[4,5,6],[7,8,9,10]]
