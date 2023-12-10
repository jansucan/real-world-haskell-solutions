-- 1. Write a function that takes two arguments: a four-element tuple and an
--    integer. With an integer argument of zero, it should return the leftmost
--    element of the tuple. With an argument of one, it should return the next
--    element. And so on. What restrictions do you have to put on the types of
--    the arguments in order to write a function that typechecks correctly?
--
-- 2. Write a similar function that takes a six-tuple as its first argument.
--
-- 3. Try refactoring the two functions to share any common code you can
--    identify. How much shared code are you able to you find?

-- In order for this function to typecheck correctly, all elements in the tuple
-- must have the same type, so the return value's type is always the same for
-- any value of the index.
get4 :: (a, a, a, a) -> Int -> Maybe a
get4 (a, _, _, _) 0 = Just a
get4 (_, b, _, _) 1 = Just b
get4 (_, _, c, _) 2 = Just c
get4 (_, _, _, d) 3 = Just d
get4 _ _ = Nothing

get6 :: (a, a, a, a, a, a) -> Int -> Maybe a
get6 (a, _, _, _, _, _) 0 = Just a
get6 (_, b, _, _, _, _) 1 = Just b
get6 (_, _, c, _, _, _) 2 = Just c
get6 (_, _, _, d, _, _) 3 = Just d
get6 (_, _, _, _, e, _) 4 = Just e
get6 (_, _, _, _, _, f) 5 = Just f
get6 _ _ = Nothing

-- Refactorring the two functions to share common code
get2 :: (a, a) -> Int -> Maybe a
get2 (a, _) 0 = Just a
get2 (_, b) 1 = Just b
get2 _ _ = Nothing

-- The 'get6' function can be refactorred to make use of the 'get4'
-- function. However, this doesn't save much code.
refGet6 :: (a, a, a, a, a, a) -> Int -> Maybe a
refGet6 t i = maybe gy (\j -> Just j) gx
  where (a, b, c, d, e, f) = t
        x = (a, b, c, d)
        y = (e, f)
        gx = get4 x i
        gy = get2 y (i - 4)


-- ghci> :l 12_a_1.hs
-- [1 of 1] Compiling Main             ( 12_a_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> get4 ('1', '2', '3', '4') 0
-- Just '1'
-- ghci> get4 ('1', '2', '3', '4') 1
-- Just '2'
-- ghci> get4 ('1', '2', '3', '4') 2
-- Just '3'
-- ghci> get4 ('1', '2', '3', '4') 3
-- Just '4'
-- ghci> get4 ('1', '2', '3', '4') 4
-- Nothing

-- ghci> get6 ('1', '2', '3', '4', '5', '6') 0
-- Just '1'
-- ghci> get6 ('1', '2', '3', '4', '5', '6') 1
-- Just '2'
-- ghci> get6 ('1', '2', '3', '4', '5', '6') 2
-- Just '3'
-- ghci> get6 ('1', '2', '3', '4', '5', '6') 3
-- Just '4'
-- ghci> get6 ('1', '2', '3', '4', '5', '6') 4
-- Just '5'
-- ghci> get6 ('1', '2', '3', '4', '5', '6') 5
-- Just '6'
-- ghci> get6 ('1', '2', '3', '4', '5', '6') 6
-- Nothing

-- ghci> refGet6 ('1', '2', '3', '4', '5', '6') 0
-- Just '1'
-- ghci> refGet6 ('1', '2', '3', '4', '5', '6') 1
-- Just '2'
-- ghci> refGet6 ('1', '2', '3', '4', '5', '6') 2
-- Just '3'
-- ghci> refGet6 ('1', '2', '3', '4', '5', '6') 3
-- Just '4'
-- ghci> refGet6 ('1', '2', '3', '4', '5', '6') 4
-- Just '5'
-- ghci> refGet6 ('1', '2', '3', '4', '5', '6') 5
-- Just '6'
-- ghci> refGet6 ('1', '2', '3', '4', '5', '6') 6
-- Nothing
