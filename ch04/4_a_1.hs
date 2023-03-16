-- Write your own "safe" definitions of the standard partial list functions, but
-- make sure that yours never fail. As a hint, you might want to consider using
-- the following types.
--
--   -- file: ch04/ch04.exercises.hs
--   safeHead :: [a] -> Maybe a
--   safeTail :: [a] -> Maybe [a]
--   safeLast :: [a] -> Maybe a
--   safeInit :: [a] -> Maybe [a]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

-- ghci> :l 4_a_1.hs
-- [1 of 1] Compiling Main             ( 4_a_1.hs, interpreted )
-- Ok, one module loaded.
-- ghci> head [1, 2, 3]
-- 1
-- ghci> head []
-- *** Exception: Prelude.head: empty list
-- ghci> safeHead [1, 2, 3]
-- Just 1
-- ghci> safeHead []
-- Nothing

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

-- ghci> tail [1, 2, 3]
-- [2,3]
-- ghci> tail []
-- *** Exception: Prelude.tail: empty list
-- ghci> safeTail [1, 2, 3]
-- Just [2,3]
-- ghci> safeTail []
-- Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- ghci> last [1, 2, 3]
-- 3
-- ghci> last []
-- *** Exception: Prelude.last: empty list
-- ghci> safeLast [1, 2, 3]
-- Just 3
-- ghci> safeLast []
-- Nothing

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- ghci> init [1, 2, 3]
-- [1,2]
-- ghci> init []
-- *** Exception: Prelude.init: empty list
-- ghci> safeInit [1, 2, 3]
-- Just [1,2]
-- ghci> safeInit []
-- Nothing
