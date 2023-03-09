-- Write a function lastButOne, that returns the element before the last.

-- This function is written in a bit cumbersome way. This is because we have
-- avoided using functions introduced in later chapters of the book (e.g.,
-- length, pattern matching). The myDrop function has been used as an example.
--
-- This function expects a list of at least two elements so the element before
-- the last one always exists.
--
-- Checking the length of the list deserves some explanation. A call to tail
-- discards the first element from the list. It is like subtracting 1 from the
-- length of the list. Let's have a list of length N. If a list of length N - 2
-- (two calls to tail) is not empty, then N is greater or equal to 3. If it is
-- empty, then N has to be 2. It should not be less than 2 because that is a
-- minimum length of the list expected.

lastButOne :: [a] -> a
lastButOne xs = if null (tail (tail xs)) -- Check if xs is 2 elements long
                then head xs
                else lastButOne (tail xs)
