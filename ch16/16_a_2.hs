-- A popular denial-of-service attack against naive web servers is simply to
-- send unreasonably long headers. A single header might contain 10s or 100s of
-- megabytes of garbage text, causing a server to run out of memory.
-- Restructure the header parser so that it will fail if any line is longer than
-- 4096 characters. It must fail immediately when this occurs; it cannot wait
-- until the end of a line eventually shows up.


{-- From examples/examples/ch16/HttpRequestParser.hs and modified --}
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Monad (liftM4)
import Data.List

maxHeaderLineLength = 4096

checkLineLength :: CharParser st ()
checkLineLength = do
  i <- getInput
  if isTooLong i
    then do
      -- It is needed to consume something from the input for the p_headers
      -- parser to fail immediately. If nothing is consumed, 'manyTill' still
      -- tries its end parser (the 'crlf').
      _ <- anyChar
      -- Set source column to make the error message more helpful, for example
      -- 'Left (line 2, column 4097)')
      pos <- getPosition
      setPosition (setSourceColumn pos (maxHeaderLineLength + 1))
      fail "Header line too long"
    else return ()
  where
    isTooLong l =  lineLen l > maxHeaderLineLength
      where
        lineLen l = length $ takeWhileMax isNotLineEnd l (maxHeaderLineLength + 1)
        takeWhileMax p xs max = take max $ takeWhile p xs
        isNotLineEnd c = (c /= '\n' && c /= '\r')

p_headers :: CharParser st [(String, String)]
p_headers = header `manyTill` crlf
  where header = checkLineLength *> (liftA2 (,) fieldName (char ':' *> spaces *> contents))
        contents = liftA2 (++) (many1 notEOL <* crlf)
                               (checkLineLength *> (continuation <|> pure []))
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

p_request :: CharParser () HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
  where q name ctor body = liftM4 HttpRequest req url p_headers body
            where req = ctor <$ string name <* char ' '
                  url = optional (char '/') *>
                        manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
                        <* crlf
{-- End of code from examples --}


testReqInfinitelyLongLine :: String
testReqInfinitelyLongLine = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "User-Agent: " ++ repeat 'u',
  "Accept: text/html",
  "",
  "Hello World!"
  ] ++ "\r\n"

testReq :: Int -> String
testReq len = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "User-Agent: " ++ take (len - 12) (repeat 'u'),
  "Accept: text/html",
  "",
  "Hello World!"
  ] ++ "\r\n"

testReqContinuation :: Int -> String
testReqContinuation len = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "User-Agent: Mozilla/5.0",
  "  continuation of user agent",
  " " ++ take (len - 1) (repeat 'u'),
  "Accept: text/html",
  "",
  "Hello World!"
  ] ++ "\r\n"


-- For brevity, longs strings are shortened by '...'

-- ghci> :l 16_a_2.hs
-- [1 of 2] Compiling Main             ( 16_a_2.hs, interpreted )
-- Ok, one module loaded.

-- ghci> runParser p_request () "" testReqInfinitelyLongLine
-- Left (line 3, column 4097):
-- Header line too long

-- ghci> runParser p_request () "" (testReq 20)
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","uuuuuuuu"),("Accept","text/html")], reqBody = Just "Hello World!\r\n"})

-- ghci> runParser p_request () "" (testReq 4096)
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","uuu...uuuuuuu"),("Accept","text/html")], reqBody = Just "Hello World!\r\n"})

-- ghci> runParser p_request () "" (testReq 4097)
-- Left (line 3, column 4097):
-- Header line too long

-- ghci> runParser p_request () "" (testReqContinuation 20)
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","Mozilla/5.0 continuation of user agent uuuuuuuuuuuuuuuuuuu"),("Accept","text/html")], reqBody = Just "Hello World!\r\n"})

-- ghci> runParser p_request () "" (testReqContinuation 4096)
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","Mozilla/5.0 continuation of user agent uuuuuu...uuuuuu"),("Accept","text/html")], reqBody = Just "Hello World!\r\n"})

-- ghci> runParser p_request () "" (testReqContinuation 4097)
-- Left (line 5, column 4097):
-- Header line too long
