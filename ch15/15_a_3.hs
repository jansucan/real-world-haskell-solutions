-- In a form-encoded string, the same key may appear several times, with or
-- without values, e.g. key&key=1&key=2. What type might you use to represent
-- the values associated with a key in this sort of string? Write a parser that
-- correctly captures all of the information.

-- I will use Map String [String] where the values are lists of values for a
-- urlencoded-key. Empty list indicates no values for the key.

-- For simplicity, I will not handle the encoding of spaces, reserved
-- characters, and non-ASCII characters in the urlencoded string.

{-- From examples/examples/ch10/Parse.hs and modified:
 --   removed unused code,
 --   made Parse an instance of Applicative,
 --   used the do notation as this chapter is "Programming with monads".
 --}
import Data.Map

data ParseState = ParseState {
      string :: String
    } deriving (Show)

newtype Parse a = Parse {
      runParse :: ParseState -> (a, ParseState)
    }

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> makeSecondParser  =  Parse chainedParser
  where chainedParser initState = runParse (makeSecondParser firstResult) newState
          where
            (firstResult, newState) = runParse firstParser initState

identity :: a -> Parse a
identity a = Parse (\s -> (a, s))

instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)

instance Applicative Parse where
  pure = identity
  parFunc <*> parVal =
    parVal ==> \v ->
    parFunc ==> \f ->
    pure (f v)

instance Monad Parse where
    return = pure
    (>>=) = (==>)

parse :: Parse a -> String -> a
parse parser initState = fst (runParse parser (ParseState initState))

getState :: Parse ParseState
getState = Parse (\s -> (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> ((), s))

parseChar :: Parse (Maybe Char)
parseChar = do
    initState <- getState
    case string initState of
      [] ->
          return Nothing
      (char:remainder) -> do
          putState newState
          return (Just char)
        where newState = initState { string = remainder }

peekChar :: Parse (Maybe Char)
peekChar = (safeHead . string) <$> getState
  where safeHead [] = Nothing
        safeHead (x:xs) = Just x

parseWhile :: (Char -> Bool) -> Parse String
parseWhile p = do
  mp <- (fmap p <$> peekChar)
  if mp == Just True
    then do
      mc <- parseChar
      case mc of
        Nothing -> return []
        Just c -> (c:) <$> parseWhile p
    else return []
{-- End of code from examples --}


parseKey :: Parse String
parseKey = parseWhile isKeyEnd
  where isKeyEnd c = (c /= '&' && c /= '=')

parseValue :: Parse String
parseValue = do
  c <- peekChar
  case c of
    Nothing  -> return ""
    Just '&' -> do
      -- No value.
      -- Consume the end character so that the next parseKey starts from the
      -- first character of the next key.
      parseChar
      return ""
    Just '=' -> do
      -- Consume the '='
      parseChar
      v <- parseWhile isValueEnd
      -- Consume the end character
      parseChar
      return v
  where isValueEnd c = (c /= '&')

parseKeyValuePairs :: Parse (Map String [String])
parseKeyValuePairs = do
  k <- parseKey
  case k of
    "" -> return empty
    _  -> do
      v <- parseValue
      case v of
        "" -> (insertWith (++) k []) <$> parseKeyValuePairs
        _  -> (insertWith (++) k [v]) <$> parseKeyValuePairs


-- ghci> :l 15_a_3.hs
-- [1 of 2] Compiling Main             ( 15_a_3.hs, interpreted )
-- Ok, one module loaded.

-- ghci> parse parseKeyValuePairs ""
-- fromList []

-- ghci> parse parseKeyValuePairs "key"
-- fromList [("key",[])]

-- ghci> parse parseKeyValuePairs "key&"
-- fromList [("key",[])]

-- ghci> parse parseKeyValuePairs "key="
-- fromList [("key",[])]

-- ghci> parse parseKeyValuePairs "key=123"
-- fromList [("key",["123"])]

-- ghci> parse parseKeyValuePairs "key=123&"
-- fromList [("key",["123"])]

-- ghci> parse parseKeyValuePairs "key&key=1&key=2"
-- fromList [("key",["1","2"])]

-- ghci> parse parseKeyValuePairs "key&foo=4&key=2&foo=&bar&key=&foo=3&key=1&foo&key=0"
-- fromList [("bar",[]),("foo",["4","3"]),("key",["2","1","0"])]
