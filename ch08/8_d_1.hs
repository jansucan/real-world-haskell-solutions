-- Glob patterns are simple enough to interpret that it's easy to write a
-- matcher directly in Haskell, rather than going through the regexp
-- machinery. Give it a try.

-- For simplifying this solution, only simple character sets are supported
-- (e.g., no character ranges).

import Text.Regex.Posix ((=~))
import Data.List

data GlobItem = Any | Set Bool [Char] | Star deriving (Show)

{-- From examples/examples/ch08/GlobRegex.hs modified according to the assignment --}
parseGlob :: String -> [GlobItem]
parseGlob "" = []

parseGlob ('*':cs) = Star : parseGlob cs

parseGlob ('?':cs) = Any : parseGlob cs

parseGlob ('[':'!':cs) = (Set True multiple) : parseGlob rest
  where (multiple, rest) = charClass [] cs
parseGlob ('[':cs)   = (Set False multiple) : parseGlob rest
  where (multiple, rest) = charClass [] cs

parseGlob (c:cs) = (Set False [c]) : parseGlob cs


charClass :: String -> String -> (String, String)
charClass multiple (']':cs) = (multiple, cs)
charClass multiple (c:cs)   = charClass (multiple ++ [c]) cs
charClass _ []              = error "unterminated character class"


matchesGlob :: String -> String -> Bool
matchesGlob glob str = matchesGlob' (parseGlob glob) str
{-- End of code from examples --}

matchesGlob' :: [GlobItem] -> String -> Bool
matchesGlob' (Any:gs) (c:cs) = matchesGlob' gs cs
matchesGlob' ((Set complement list):gs) (c:cs) = if doesMatch
                                                 then matchesGlob' gs cs
                                                 else False
  where doesMatch = if complement
                    then all (\x -> x /= c) list
                    else any (\x -> x == c) list
matchesGlob' (Star:gs) cs = any (\x -> matchesGlob' gs x) (reverse (tails cs))

matchesGlob' [] [] = True
matchesGlob' [] (c:cs) = False
matchesGlob' (g:gs) [] = False


-- ghci> :l 8_d_1.hs
-- [1 of 1] Compiling Main             ( 8_d_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> matchesGlob "" "hello"
-- False

-- ghci> matchesGlob "h?llo" "hello"
-- True

-- ghci> matchesGlob "h?ll" "hello"
-- False

-- ghci> matchesGlob "h?llo" "hello!"
-- False

-- ghci> matchesGlob "h[eCD]llo" "hello"
-- True

-- ghci> matchesGlob "h[!eCD]llo" "hello"
-- False

-- ghci> matchesGlob "h[!XYz]llo" "hello"
-- True

-- ghci> matchesGlob "*" "hello"
-- True

-- ghci> matchesGlob "he*o" "hello"
-- True

-- ghci> matchesGlob "he*" "hello"
-- True

-- ghci> matchesGlob "he*y" "hello"
-- False

-- ghci> matchesGlob "h*e*o" "hello"
-- True

-- ghci> matchesGlob "[!abc][eFG]*Y" "hello"
-- False

-- ghci> matchesGlob "[!abc][eFG]*o" "hello"
-- True

-- ghci> matchesGlob "a*c" "abcabc"
-- True
