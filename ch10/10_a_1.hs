-- 1. Write a parser for "plain" PGM files.
--
-- 2. In our description of "raw" PGM files, we omitted a small detail. If the
--    "maximum grey" value in the header is less than 256, each pixel is
--    represented by a single byte. However, it can range up to 65,535, in which
--    case, each pixel will be represented by 2 bytes, in big-endian order (most
--    significant byte first).
--
--    Rewrite the raw PGM parser to accommodate both the single- and double-byte
--    pixel formats.
--
-- 3. Extend your parser so that it can identify a raw or plain PGM file, and parse
--    the appropriate file type.


{-- From examples/examples/ch10/Parse.hs modified according to the assignment --}
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)


data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b

firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState


getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h

parseRawPGM :: Int -> WordSize -> Parse [Int]
parseRawPGM pixelCount wordSize =
    parseByte ==>&
    parseBytes byteCount ==> \bitmap ->
    identity (wordsToInts bitmap)
  where (byteCount, wordsToInts) = if wordSize == WordSize16bit
                                   then ((2 * pixelCount), words8ToInts16)
                                   else (pixelCount, words8ToInts8)
{-- End of code from examples --}


{-- From examples/examples/ch10/Parse.hs modified according to the assignment --}
data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: [Int]
    } deriving (Eq)
{-- End of code from examples --}

type PGMType = String

data WordSize = WordSize8bit | WordSize16bit deriving (Eq)

instance Show Greymap where
    show (Greymap w h m d) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m ++ " " ++ show d


words8ToInts8 :: L.ByteString -> [Int]
words8ToInts8 words =
  case (L.uncons words) of
    Nothing       -> []
    Just (w, rem) -> (fromIntegral w) : (words8ToInts8 rem)


words8ToInts16 :: L.ByteString -> [Int]
words8ToInts16 words = ints8ToInts16 (words8ToInts8 words)
  where
    ints8ToInts16 (a:b:rem) = ((a * 256) + b) : (ints8ToInts16 rem)
    ints8ToInts16 _ = []


is16Bit :: Greymap -> Bool
is16Bit (Greymap _ _ m _) = (m > 255)


parseHeaderPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    identity (header, (Greymap width height maxGrey []))
  where notWhite = (`notElem` " \r\n\t")


parsePlainPixel :: Parse Int
parsePlainPixel = skipSpaces ==>& parseNat


parsePlainPGM :: Int -> Parse [Int]
parsePlainPGM pixelCount =
  if pixelCount == 0
  then
    identity []
  else
    parsePlainPixel ==> \pixelValue ->
    (pixelValue:) <$> parsePlainPGM (pixelCount - 1)


parsePGMFile :: FilePath -> IO (Either String Greymap)
parsePGMFile filePath = do
  str <- L.readFile filePath
  let state = ParseState str 0
  case (runParse parseHeaderPGM) state of
    Left err              -> return (Left err)
    Right (header, state) -> return $ parsePGMFile' header state
  where
    parsePGMFile' :: (PGMType, Greymap) -> ParseState -> Either String Greymap
    parsePGMFile' (pgmType, greymap) state
      | pgmType == "P2"                    = parse (parsePlainPGM pixelCount) state
      | pgmType == "P5" && is16Bit greymap = parse (parseRawPGM pixelCount WordSize16bit) state
      | pgmType == "P5"                    = parse (parseRawPGM pixelCount WordSize8bit) state
      | otherwise                          = Left "Unknown PGM type"
      where (Greymap width height maxGrey _) = greymap
            pixelCount = width * height
            parse parser state = case (runParse parser) state of
                                   Left err -> Left err
                                   Right (body, state) -> Right (Greymap width height maxGrey body)


-- ghci> :l 10_a_1.hs
-- [1 of 1] Compiling Main             ( 10_a_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> parsePGMFile "test-10_a_1/plain.pgm"
-- Right Greymap 3x3 255 [0,12,65,101,143,192,200,224,255]

-- ghci> parsePGMFile "test-10_a_1/raw_8bit.pgm"
-- Right Greymap 3x3 255 [0,12,65,101,143,192,200,224,255]

-- ghci> parsePGMFile "test-10_a_1/raw_16bit.pgm"
-- Right Greymap 3x3 65535 [0,3073,16642,25859,36612,49157,51206,57351,65535]
