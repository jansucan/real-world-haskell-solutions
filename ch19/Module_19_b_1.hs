-- Write a many parser, with type Parser a -> Parser [a]. It should apply a
-- parser until it fails.

-- Control.Monad.Error is deprecated. Control.Monad.Except should be used
-- instead. I modified the code from examples here to use it.


{-- From examples/examples/ch19/ParseInt.hs and modified --}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Module_19_b_1 where

import Control.Monad.Except
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                  deriving (Eq, Ord, Show)

newtype Parser a = P {
      runP :: ExceptT ParseError (State B.ByteString) a
    } deriving (Functor, Applicative, Monad, MonadError ParseError)

liftP :: State B.ByteString a -> Parser a
liftP m = P (lift m)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP get
  case B.uncons s of
    Nothing         -> throwError EndOfInput
    Just (c, s')
        | p c       -> liftP (put s') >> return c
        | otherwise -> throwError (Chatty "satisfy failed")

runParser :: Parser a -> B.ByteString
          -> Either ParseError (a, B.ByteString)
runParser p bs = case runState (runExceptT (runP p)) bs of
                   (Left err, _) -> Left err
                   (Right r, bs) -> Right (r, bs)

optional :: Parser a -> Parser (Maybe a)
optional p = (Just `liftM` p) `catchError` \_ -> return Nothing
{-- End of code from examples --}


many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) `catchError` \_ -> return []


-- ghci> :l Module_19_b_1.hs
-- [1 of 1] Compiling Module_19_b_1    ( Module_19_b_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> :m +Data.Char

-- ghci> runParser (many (satisfy isDigit)) (B.pack "")
-- Right ("","")

-- ghci> runParser (many (satisfy isDigit)) (B.pack "abc")
-- Right ("","abc")

-- ghci> runParser (many (satisfy isDigit)) (B.pack "9abc")
-- Right ("9","abc")

-- ghci> runParser (many (satisfy isDigit)) (B.pack "987abc")
-- Right ("987","abc")

-- ghci> runParser (many (satisfy isDigit)) (B.pack "x987abc")
-- Right ("","x987abc")
