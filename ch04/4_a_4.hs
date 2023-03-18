-- Write a program that transposes the text in a file. For instance, it should
-- convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

{-- From examples/examples/ch04/InteractWith.hs --}
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = transpose
{-- End of code from examples --}

-- The assignment doesn't mention that the implementation should handle rows of
-- different lengths, and the example input contains only rows of the same
-- length. Thus, for simplicity, we assume that all rows of the input have the
-- same length.

firstColumn :: [String] -> String
firstColumn [] = ""
firstColumn (x:xs)
  | null x = ""
  | otherwise = (head x):(firstColumn xs)

removeFirstColumn :: [String] -> [String]
removeFirstColumn [] = []
removeFirstColumn (x:xs) = (tail x):(removeFirstColumn xs)

transpose' :: [String] -> [String]
transpose' rows = if null column
                  then []
                  else column:(transpose' rest)
  where column = firstColumn rows
        rest = removeFirstColumn rows

transpose input = unlines (transpose' (lines input))

-- $ stack ghc 4_a_4.hs
-- [1 of 1] Compiling Main             ( 4_a_4.hs, 4_a_4.o )
-- Linking 4_a_4 ...
-- $ cat input.txt
-- hello
-- world
-- $ ./4_a_4 input.txt output.txt
-- $ cat output.txt
-- hw
-- eo
-- lr
-- ll
-- od
