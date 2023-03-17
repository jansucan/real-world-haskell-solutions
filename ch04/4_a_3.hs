-- Using the command framework from the earlier section "A simple command line
-- framework" on page 71, write a program that prints the first word of each
-- line of its input.

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
        myFunction = firstWords
{-- End of code from examples --}

firstWords' :: [String] -> [String]
firstWords' [] = []
firstWords' (x:xs) = first:(firstWords' xs)
  where lineWords = words x
        first = if not (null lineWords)
                then head lineWords
                else ""

firstWords input = unlines (firstWords' (lines input))

-- $ stack ghc 4_a_3.hs
-- [1 of 1] Compiling Main             ( 4_a_3.hs, 4_a_3.o )
-- Linking 4_a_3 ...
-- $ cat input.txt
-- ab cde fghi
--
--   j   k
--
-- lm
-- $ ./4_a_3 input.txt output.txt
-- $ cat output.txt
-- ab
--
-- j
--
-- lm
