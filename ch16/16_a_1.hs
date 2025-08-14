-- Our HTTP request parser is too simple to be useful in real deployments. It is
-- missing vital functionality and is not resistant to even the most basic
-- denial-of-service attacks.
-- Make the parser honor the Content-Length field properly, if it is present.


{-- From examples/examples/ch16/HttpRequestParser.hs and modified --}

-- No need to import ApplicativeParsec with custom Applicative and Alternative instances
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import Numeric (readDec)
import Control.Monad (liftM4, liftM2)

import Data.List -- For easier definition of test requests

type UserStateContentLength = Integer

unlimitedContentLength :: Integer
unlimitedContentLength = -1

p_headers :: CharParser UserStateContentLength [(String, String)]
p_headers = ((try contentLength) <|> header) `manyTill` crlf
  where contentLength = liftA2 (,) (string "Content-Length") (char ':' *> spaces *> contentLengthValue)
        contentLengthValue = do
          s <- (many1 digit <* crlf)
          case readDec s of
            [(n, _)] -> do
              setState n
              return s
            _ -> return s
        header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
        contents = liftA2 (++) (many1 notEOL <* crlf)
                               (continuation <|> pure [])
        continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
        fieldName = (:) <$> letter <*> many fieldChar
        fieldChar = letter <|> digit <|> oneOf "-_"

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

data Method = Get | Post
          deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest {
      reqMethod :: Method
    , reqURL :: String
    , reqHeaders :: [(String, String)]
    , reqBody :: Maybe String
    } deriving (Eq, Show)

p_request :: CharParser UserStateContentLength HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> getBody)
  where q name ctor body = liftM4 HttpRequest req url p_headers body
            where req = ctor <$ string name <* char ' '
        url = optional (char '/') *>
              manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
              <* crlf
        getBody = do
          len <- getState
          if len >= 0
            then getBodyLength len
            else many anyChar -- Unlimited body length
        getBodyLength 0 = return []
        getBodyLength n = (eof *> return []) <|> (liftM2 (:) anyChar (getBodyLength (n-1)))
{-- End of code from examples --}


testReqNoContentLength :: String
testReqNoContentLength = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "User-Agent: Mozilla/5.0",
  "Accept: text/html",
  "",
  "Hello World!"
  ] ++ "\r\n"

testReqContentLength :: Integer -> String
testReqContentLength len = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "User-Agent: Mozilla/5.0",
  "Accept: text/html",
  "Content-Length: " ++ show len,
  "",
  "Hello World!"
  ] ++ "\r\n"


-- ghci> :l 16_a_1.hs
-- [1 of 2] Compiling Main             ( 16_a_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> runParser p_request unlimitedContentLength "" testReqNoContentLength
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","Mozilla/5.0"),("Accept","text/html")], reqBody = Just "Hello World!\r\n"})

-- ghci> runParser p_request unlimitedContentLength "" (testReqContentLength 0)
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","Mozilla/5.0"),("Accept","text/html"),("Content-Length","0")], reqBody = Just ""})

-- ghci> runParser p_request unlimitedContentLength "" (testReqContentLength 7)
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","Mozilla/5.0"),("Accept","text/html"),("Content-Length","7")], reqBody = Just "Hello W"})

-- ghci> runParser p_request unlimitedContentLength "" (testReqContentLength 123)
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","Mozilla/5.0"),("Accept","text/html"),("Content-Length","123")], reqBody = Just "Hello World!\r\n"})
