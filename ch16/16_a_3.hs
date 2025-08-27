-- Add the ability to honor the 'Transfer-Encoding: chunked' header if it is
-- present. See section 3.6.1 of RFC 2616
-- (http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6.1) for details.

-- To shorten and simplify the implementation, I treat trailer entity headers as
-- just generic message headers.


{-- From examples/examples/ch16/HttpRequestParser.hs and modified --}
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), token)

import Numeric (readHex)
import Control.Monad (liftM4)
import Data.Char (chr)

import Data.List (intercalate)

type UserStateIsEncodingChunked = Bool

defaultIsEncodingChunked = False

p_headers :: CharParser UserStateIsEncodingChunked [(String, String)]
p_headers = ((try transferEncoding) <|> p_messageHeader) `manyTill` crlf
  where transferEncoding = liftA2 (,) (string "Transfer-Encoding")
                                      (char ':' *> spaces *> string "chunked" <* setState True <* crlf)

p_messageHeader :: CharParser st (String, String)
p_messageHeader = liftA2 (,) fieldName (char ':' *> spaces *> contents)
  where
    contents = liftA2 (++) (many1 notEOL <* crlf)
                           (continuation <|> pure [])
    continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
    fieldName = (:) <$> letter <*> many fieldChar
    fieldChar = letter <|> digit <|> oneOf "-_"

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

data Method = Get | Post
          deriving (Eq, Ord, Show)

data HttpBody = BodyIdentity String | BodyChunked [Chunk] [(String, String)]
          deriving (Eq, Show)

data Chunk = Chunk {
      chunkExtensions :: [(String, String)]
    , chunkData :: String
    } deriving (Eq, Show)

data HttpRequest = HttpRequest {
      reqMethod :: Method
    , reqURL :: String
    , reqHeaders :: [(String, String)]
    , reqBody :: Maybe HttpBody
    } deriving (Eq, Show)

p_request :: CharParser UserStateIsEncodingChunked HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> p_body)
  where q name ctor body = liftM4 HttpRequest req url p_headers body
            where req = ctor <$ string name <* char ' '
        url = optional (char '/') *>
              manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
              <* crlf
{-- End of code from examples --}


p_body :: CharParser UserStateIsEncodingChunked HttpBody
p_body = do
  chunked <- getState
  if chunked
    then p_bodyChunked
    else BodyIdentity <$> many anyChar

p_bodyChunked :: CharParser UserStateIsEncodingChunked HttpBody
p_bodyChunked = BodyChunked <$> p_allChunks <*> p_trailer <* crlf

p_allChunks :: CharParser st [Chunk]
p_allChunks = do
  size <- chunkSize
  if size > 0
    then (:) <$> (p_chunk size) <*> p_allChunks
    else (\a -> [a]) <$> p_lastChunk
  where chunkSize = do
          s <- many1 hexDigit
          -- readHex cannot fail here because at least one hex digit has been read
          case readHex s of
            [(n, _)] -> return n

p_chunk :: Int -> CharParser st Chunk
p_chunk size = do
  exts <- p_allChunkExtensions
  chunkData <- ((count size (oneOf octetChars)) <* crlf)
  return (Chunk exts chunkData)

p_lastChunk :: CharParser st Chunk
p_lastChunk = do
  exts <- p_allChunkExtensions
  return (Chunk exts [])

p_allChunkExtensions :: CharParser st [(String, String)]
p_allChunkExtensions = chunkExtension `manyTill` crlf
  where
    chunkExtension = (,) <$> (char ';' *> chunkExtName) <*> (option "" chunkExtVal)
    chunkExtName = token
    chunkExtVal = char '=' *> (quotedString <|> token)

p_trailer :: CharParser st [(String, String)]
p_trailer = many p_messageHeader

sp :: CharParser st Char
sp = char spChar

ht :: CharParser st Char
ht = char htChar

separator :: CharParser st Char
separator = oneOf separatorChars

ctl :: CharParser st Char
ctl = oneOf controlChars <|> char delChar

crlf :: CharParser st String
crlf = string crlfChars

lws :: CharParser st String
lws = (++) <$> (option "" crlf) <*> many1 (sp <|> ht)

quotedPair :: CharParser st String
quotedPair =  join <$> char '\\' <*> oneOf charChars
  where join a b = a : [b]

qdtext :: CharParser st String
qdtext = (\a -> [a]) <$> oneOf (textChars `without` "\"")

quotedString :: CharParser st String
quotedString =  join <$> startEnd <*> middle <*> startEnd
  where join a b c = a ++ b ++ c
        startEnd = string "\""
        middle = concat <$> (many (quotedPair <|> qdtext))

token :: CharParser st String
token = many1 $ oneOf tokenChars

spChar = ' '
htChar = chr 9
delChar = chr 127

crlfChars = "\r\n"
octetChars = charRange 0 255
charChars = charRange 0 127
controlChars = charRange 0 31
separatorChars = spChar : htChar : "()<>@,;:\\\"/[]?={}"
textChars = octetChars `without` controlChars
tokenChars = (charChars `without` controlChars) `without` separatorChars
qdtextChars = textChars `without` "\""

charRange :: Int -> Int -> String
charRange start end
  | start > end = ""
  | otherwise   = chr start : charRange (start + 1) end

without :: String -> String -> String
without [] _            = []
without (c:cs) excluded = x ++ (without cs excluded)
  where x = if c `elem` excluded
            then []
            else [c]




testReqNotChunked :: String
testReqNotChunked = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "User-Agent: Mozilla/5.0",
  "Accept: text/html",
  "",
  "Hello World!"
  ]

testReqChunkedWithExtensions :: String
testReqChunkedWithExtensions = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "Transfer-Encoding: chunked",
  "",
  "18",
  "Chunk without extensions",
  "22;extension",
  "Chunk with extension without value",
  "1f;extension=value",
  "Chunk with extension with value",
  "2d;extension=\"quoted string value\"",
  "Chunk with extension with quoted string value",
  "24;a;b=valueB;c=\"value C\"",
  "Chunk with all extension value types",
  "0",
  "",
  ""
  ]

testReqChunkedExtensionsInTheLastChunk :: String
testReqChunkedExtensionsInTheLastChunk = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "Transfer-Encoding: chunked",
  "",
  "5",
  "Chunk",
  "0;a;b=valueB;c=\"value C\"",
  "",
  ""
  ]

testReqTrailer :: String
testReqTrailer = intercalate "\r\n" [
  "POST /index.html HTTP/1.1",
  "Host: book.realworldhaskell.org",
  "Transfer-Encoding: chunked",
  "",
  "5",
  "Chunk",
  "0",
  "trailer: value",
  "trailerB: valueB",
  "",
  ""
  ]

-- The outputs from parsing the test requests are manually reformatted to make
-- them easier to read and compare to the requests

-- ghci> :l 16_a_3.hs
-- [1 of 2] Compiling Main             ( 16_a_3.hs, interpreted )
-- Ok, one module loaded.

-- ghci> runParser p_request defaultIsEncodingChunked "" testReqNotChunked
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html",
--                     reqHeaders = [("Host","book.realworldhaskell.org"),
--                                   ("User-Agent","Mozilla/5.0"),
--                                   ("Accept","text/html")],
--                     reqBody = Just (BodyIdentity "Hello World!")})

-- ghci> runParser p_request defaultIsEncodingChunked "" testReqChunkedWithExtensions
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html",
--                     reqHeaders = [("Host","book.realworldhaskell.org"),
--                                   ("Transfer-Encoding","chunked")],
--                     reqBody = Just (BodyChunked [Chunk {chunkExtensions = [],
--                                                         chunkData = "Chunk without extensions"},
--                                                  Chunk {chunkExtensions = [("extension","")],
--                                                         chunkData = "Chunk with extension without value"},
--                                                  Chunk {chunkExtensions = [("extension","value")],
--                                                         chunkData = "Chunk with extension with value"},
--                                                  Chunk {chunkExtensions = [("extension","\"quoted string value\"")],
--                                                         chunkData = "Chunk with extension with quoted string value"},
--                                                  Chunk {chunkExtensions = [("a",""),
--                                                                            ("b","valueB"),
--                                                                            ("c","\"value C\"")],
--                                                         chunkData = "Chunk with all extension value types"},
--                                                  Chunk {chunkExtensions = [],
--                                                         chunkData = ""}] [])})

-- ghci> runParser p_request defaultIsEncodingChunked "" testReqChunkedExtensionsInTheLastChunk
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html",
--                     reqHeaders = [("Host","book.realworldhaskell.org"),
--                                   ("Transfer-Encoding","chunked")],
--                     reqBody = Just (BodyChunked [Chunk {chunkExtensions = [],
--                                                         chunkData = "Chunk"},
--                                                  Chunk {chunkExtensions = [("a",""),
--                                                                            ("b","valueB"),
--                                                                            ("c","\"value C\"")],
--                                                          chunkData = ""}] [])})

-- ghci> runParser p_request defaultIsEncodingChunked "" testReqTrailer
-- Right (HttpRequest {reqMethod = Post, reqURL = "index.html",
--                     reqHeaders = [("Host","book.realworldhaskell.org"),
--                                   ("Transfer-Encoding","chunked")],
--                     reqBody = Just (BodyChunked [Chunk {chunkExtensions = [], chunkData = "Chunk"},
--                                                  Chunk {chunkExtensions = [], chunkData = ""}]
--                                                 [("trailer","value"),
--                                                  ("trailerB","valueB")])})
