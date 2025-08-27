-- Another popular attack is to open a connection and either leave it idle or
-- send data extremely slowly.
-- Write a wrapper in the IO monad that will invoke the parser. Use the
-- System.Timeout module to close the connection if the parser does not complete
-- within 30 seconds.

-- The chapter doesn't use network connections in the parsing examples. Closing
-- the connection probably means stopping the parse so it doesn't block for a
-- long time.


import Control.Concurrent (threadDelay)
import System.Timeout (timeout)
import Data.List (intercalate)


{-- From examples/examples/ch16/HttpRequestParser.hs --}
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Monad (liftM4)

p_headers :: CharParser st [(String, String)]
p_headers = header `manyTill` crlf
  where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
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

p_request :: CharParser () HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
  where q name ctor body = liftM4 HttpRequest req url p_headers body
            where req = ctor <$ string name <* char ' '
        url = optional (char '/') *>
              manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
              <* crlf
{-- End of code from examples --}


parseWithTimeout parser request = timeout timeoutMicrosec parse
  where
    timeoutMicrosec = 30 * 10^6 -- 30 seconds
    parse = do
      req <- request
      let res = runParser p_request () "" req
      return res




testRequestWithDelay :: Int -> IO String
testRequestWithDelay delayMicrosec = do
  let start = intercalate "\r\n" [
        "POST /index.html HTTP/1.1",
        "Host: book.realworldhaskell.org",
        "User-Agent: Mozilla/5.0",
        "Accept: text/html",
        "",
        "Hello "
        ]
  threadDelay delayMicrosec
  return (start ++ "World!")

testRequestFast = testRequestWithDelay (25 * 10^6)

testRequestSlow = testRequestWithDelay (32 * 10^6)


-- ghci> :l 16_a_4.hs
-- [1 of 2] Compiling Main             ( 16_a_4.hs, interpreted )
-- Ok, one module loaded.

-- ghci> parseWithTimeout p_request testRequestFast
-- Just (Right (HttpRequest {reqMethod = Post, reqURL = "index.html", reqHeaders = [("Host","book.realworldhaskell.org"),("User-Agent","Mozilla/5.0"),("Accept","text/html")], reqBody = Just "Hello World!"}))

-- ghci> parseWithTimeout p_request testRequestSlow
-- Nothing
