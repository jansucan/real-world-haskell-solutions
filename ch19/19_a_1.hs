-- Take the Either example and made it work with laziness in the style of the
-- Maybe example.


-- I chose the non-Monadic version of the Either example because the monadic one
-- required changes to make it work with recent GHC versions.

{-- From examples/examples/ch19/divby7.hs and modified --}
data DivByError a = DivBy0
                 | ForbiddenDenominator a
                   deriving (Eq, Read, Show)

divBy :: Integral a => a -> [a] -> [Either (DivByError a) a]
divBy numerator denominators =
  map worker denominators
  where worker 0 = Left DivBy0
        worker 10 = Left (ForbiddenDenominator 10)
        worker 20 = Left (ForbiddenDenominator 20)
        worker x = Right (numerator `div` x)
{-- End of code from examples --}


-- ghci> :l 19_a_1.hs
-- [1 of 2] Compiling Main             ( 19_a_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> take 8 (divBy 50 [1..])
-- [Right 50,Right 25,Right 16,Right 12,Right 10,Right 8,Right 7,Right 6]

-- ghci> divBy 50 [1,0,5,10,30,20]
-- [Right 50,Left DivBy0,Right 10,Left (ForbiddenDenominator 10),Right 1,Left (ForbiddenDenominator 20)]
