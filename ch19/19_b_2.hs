-- 1. Use many to write an int parser, with type Parser Int. It should accept
--    negative as well as positive integers.
--
-- 2. Modify your int parser to throw a NumericOverflow exception if it detects
--    a numeric overflow while parsing.


-- For simplicity, I use NumericOverflow also for indicating underflow as the
-- ParseError doesn't contain it and I didn't want to modify it in the module of
-- the previous exercise.


import Module_19_b_1

import qualified Data.ByteString.Char8 as B -- For testing

import Data.Char
import Control.Monad.Except
import Control.Monad.State

parseIntStr :: Parser String
parseIntStr = do
  -- It's not possible to decide about success or error only from the first
  -- char. When the string starts with '-', then it depends on what is following
  -- it: digit is success, not digit is error.
  -- Save the the input string so it can be restored in the case of error.
  s <- liftP get

  -- 'satisfy' doesn't consume from the input in the case of error. No need to
  -- restore it before throwing error.
  first <- satisfy (\x -> x == '-' || isDigit x) `catchError` \_ -> throwError notAnIntError
  rest <- many $ satisfy isDigit
  if first == '-' && null rest
    then
      -- Error: Minus char is not followed by any digits. Restore the input (the
      -- '-' char).
      liftP (put s) >> throwError notAnIntError
    else return $ first : rest
  where
    notAnIntError = Chatty "not an Int"

parseInt :: Parser Int
parseInt = do
  str <- parseIntStr
  let i = read str :: Int
  -- Detect overflow/underflow by doing 'show.read' roundtrip. If the Int-string
  -- is too long (too big positive or negative number), the 'read' function
  -- still converts it successfully, but with overflow/underflow. If 'show'
  -- doesn't produce back the original Int-string, then overflow/underflow
  -- happened.
  if show i == str
    then return i
    else throwError NumericOverflow




-- Parse Int-string or the first two chars. When parsing "-abc" fails, the "-a"
-- should still stay in the input and be parsed and returned by this parser.
testRestore :: Parser String
testRestore = do
  parseIntStr `catchError` \_ -> (\x y -> [x, y]) <$> satisfy allChar <*> satisfy allChar
  where
    allChar _ = True


-- ghci> :l 19_b_2.hs
-- [1 of 3] Compiling Module_19_b_1    ( Module_19_b_1.hs, interpreted )
-- [2 of 3] Compiling Main             ( 19_b_2.hs, interpreted )
-- Ok, two modules loaded.

-- ghci> runParser testRestore (B.pack "-abc")
-- Right ("-a","bc")

-- ghci> runParser parseInt  (B.pack "")
-- Left (Chatty "not an Int")

-- ghci> runParser parseInt  (B.pack "-")
-- Left (Chatty "not an Int")

-- ghci> runParser parseInt  (B.pack "-abc")
-- Left (Chatty "not an Int")

-- ghci> runParser parseInt  (B.pack "-123")
-- Right (-123,"")

-- ghci> runParser parseInt  (B.pack "-123abc")
-- Right (-123,"abc")

-- ghci> runParser parseInt  (B.pack "-1")
-- Right (-1,"")

-- ghci> runParser parseInt  (B.pack "0")
-- Right (0,"")

-- ghci> runParser parseInt  (B.pack "1")
-- Right (1,"")

-- ghci> runParser parseInt  (B.pack "abc")
-- Left (Chatty "not an Int")

-- ghci> runParser parseInt  (B.pack "123")
-- Right (123,"")

-- ghci> runParser parseInt  (B.pack "123abc")
-- Right (123,"abc")

-- ghci> runParser parseInt (B.pack $ show ((toInteger (minBound :: Int)) - 1))
-- Left NumericOverflow

-- ghci> runParser parseInt (B.pack $ show ((toInteger (minBound :: Int)) ))
-- Right (-9223372036854775808,"")

-- ghci> runParser parseInt (B.pack $ show ((toInteger (maxBound :: Int)) ))
-- Right (9223372036854775807,"")

-- ghci> runParser parseInt (B.pack $ show ((toInteger (maxBound :: Int)) + 1))
-- Left NumericOverflow
