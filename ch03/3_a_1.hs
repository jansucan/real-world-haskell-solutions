-- Write the converse of fromList for the List type: a function that takes a
-- List a and generates a [a].

{-- From examples/examples/ch03/ListADT.hs --}
data List a = Cons a (List a)
            | Nil
              deriving (Show)
{-- End of code from examples --}

toList (Cons x xs) = x:(toList xs)
toList Nil         = []

-- ghci> toList (Cons 1 (Cons 2 (Cons 3 Nil)))
-- [1,2,3]
