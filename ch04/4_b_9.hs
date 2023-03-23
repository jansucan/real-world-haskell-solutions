-- 1. The Data.List module defines a function, groupBy, which has the following
--    type:
--
--      -- file: ch04/ch04.exercises.hs
--      groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
--
-- 2. Use ghci to load the Data.List module and figure out what groupBy does,
--    then write your own implementation using a fold.

-- NOTE:
--
-- When I was solving this exercise for the first time, I tried the grouBy
-- function only with (==) operator and came to a wrong conclusion about how it
-- works. I thought that it compared current item to the previous one. After I
-- had the solution implemented, I tried it with (<) operator and found a
-- surprising result. That made me quite unsure about its behavior so I checked
-- the Haskell documentation; groupBy actually compares to the first element in
-- the group.
--
-- For completeness, I kept the wrong solution here. Then I went through this
-- exercise again solving it as if I found the correct behavior only from trying
-- different comparison operators and inputs.
--
-- Note that in general it is practically impossible to find out complete
-- behavior of a function as a black box. We can only get closer to it, but we
-- can never be completely sure without checking its implementation or reading a
-- trusted documentation. For example, even though from
--
--   ghci> groupBy (<) [1, 2, 3, 2]
--   [[1,2,3,2]]
--   ghci> groupBy (<) [1, 2, 3, 1]
--   [[1,2,3],[1]]
--
-- we could say that it compares to the first item in the group, but what if it
-- compares like that only for a group up to 3 elements, and to the second item
-- for a group of 4 and more elements. We could prove this by more experiments,
-- but for an input list of unlimited size, we could come up with infinite
-- amount of possible behaviors.

wrongMyGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
wrongMyGroupBy cmp xs = foldr op [] xs
  where headCmp x (a:_) = cmp x a
        op x [] = [[x]]
        op x (a:as) = if headCmp x a
                      then (x:a):as -- x is equal to the the head of the most
                                    -- recent list (thus equal to all its items).
                                    -- Add it to that list.
                      else [x]:(a:as) -- x is not equal the head of the most
                                      -- recent list. Start a new list.

-- Let's try to find out what groupBy does by checking its type and output for
-- different inputs.

-- ghci> import Data.List
-- ghci> :t groupBy
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- ghci> groupBy (==) []
-- []
-- ghci> groupBy (==) [1, 2, 3, 4, 5, 6]
-- [[1],[2],[3],[4],[5],[6]]
-- ghci> groupBy (==) [1, 1, 3, 4, 5, 6]
-- [[1,1],[3],[4],[5],[6]]
-- ghci> groupBy (==) [1, 1, 3, 4, 4, 6]
-- [[1,1],[3],[4,4],[6]]
-- ghci> groupBy (<) [1, 2, 3, 2]
-- [[1,2,3,2]]
-- ghci> groupBy (<) [1, 2, 3, 1]
-- [[1,2,3],[1]]
-- ghci> groupBy (<) [1, 2, 0, 1]
-- [[1,2],[0,1]]

-- groupBy seems to partition a list into sublists of adjacent items where all
-- items in a sublist except the first one are compared with a 'true' result to
-- the first item in that sublist.

-- Left fold makes the implementation much easier because every sublist always
-- (since it is added) contains the first value to compare the other values
-- to. This is not the case with the right fold. When folding from the right and
-- processing the elements one by one, we don't know which element is the first
-- one to compare the other values in a sublist to, so processing an element may
-- require splitting or merging the previous sublists.

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy cmp xs = foldl op [] xs
  where headCmp (a:_) x = cmp a x
        op [] x = [[x]]
        op as x = if headCmp (last as) x
                  then (init as) ++ [(last as) ++ [x]]
                  else as ++ [[x]]

-- ghci> :l 4_b_9.hs
-- [1 of 1] Compiling Main             ( 4_b_9.hs, interpreted )
-- Ok, one module loaded.
-- ghci> myGroupBy (==) []
-- []
-- ghci> myGroupBy (==) [1, 2, 3, 4, 5, 6]
-- [[1],[2],[3],[4],[5],[6]]
-- ghci> myGroupBy (==) [1, 1, 3, 4, 5, 6]
-- [[1,1],[3],[4],[5],[6]]
-- ghci> myGroupBy (==) [1, 1, 3, 4, 4, 6]
-- [[1,1],[3],[4,4],[6]]
-- ghci> myGroupBy (<) [1, 2, 3, 2]
-- [[1,2,3,2]]
-- ghci> myGroupBy (<) [1, 2, 3, 1]
-- [[1,2,3],[1]]
-- ghci> myGroupBy (<) [1, 2, 0, 1]
-- [[1,2],[0,1]]
