-- While filesystems on Unix are usually case-sensitive (e.g. “G” vs. “g”) in
-- file names, Windows filesystems are not. Add a parameter to the globToRegex
-- and matchesGlob functions that allows control over case sensitive matching.

-- For simplicity, let's assume that only a-z and A-Z letters are affected by
-- the case sensitivity option. Also, probably not all error cases are detected
-- when converting a glob to regex.

{-- From examples/examples/ch08/GlobRegex.hs modified according to the assignment --}
import Data.Char
import Text.Regex.Posix ((=~))

data MatchOptions = DontIgnoreCase | IgnoreCase

globToRegex :: String -> MatchOptions -> String
globToRegex cs opt = '^' : globToRegex' cs opt ++ "$"

globToRegex' :: String -> MatchOptions -> String
globToRegex' "" _ = ""
globToRegex' ('*':cs) opt = ".*" ++ globToRegex' cs opt
globToRegex' ('?':cs) opt = '.' : globToRegex' cs opt
globToRegex' ('[':'!':cs) opt = "[^" ++ charClass cs opt
globToRegex' ('[':cs) opt     = '[' : charClass cs opt
globToRegex' (c:cs) opt       = escape c opt ++ globToRegex' cs opt

escape :: Char -> MatchOptions -> String
escape c opt | c `elem` regexChars = '\\' : [c]
             | otherwise = caseOutsideCharClass c opt
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> MatchOptions -> String
charClass (']':cs) opt     = ']' : globToRegex' cs opt
charClass (a:'-':c:cs) opt = caseRange a c opt ++ charClass cs opt
charClass (c:cs) opt       = (caseInsideCharClass c opt) ++ charClass cs opt
charClass [] _             = error "unterminated character class"


matchesGlob :: FilePath -> String -> MatchOptions -> Bool
matchesGlob name pat opt = name =~ (globToRegex pat opt)
{-- End of code from examples --}

oppositeCase :: Char -> Char
oppositeCase c = if isAlpha c
                 then if isLower c
                      then toUpper c
                      else toLower c
                 else error (c:" is not a caseable character")

caseRange :: Char -> Char -> MatchOptions -> String
caseRange a c DontIgnoreCase = a : '-' : [c]
caseRange a c IgnoreCase = if sameCase
                                   then a:'-':c:opA:'-':[opC]
                                   -- Patterns like W-d which means W-Za-d are
                                   -- converted like w-za-dW-ZA-D
                                   else opA:"-za-" ++ c:a:"-ZA-" ++ [opC]
  where sameCase = ((isLower a) && (isLower c)) || ((isUpper a) && (isUpper c))
        opA = oppositeCase a
        opC = oppositeCase c

caseInsideCharClass :: Char -> MatchOptions -> String
caseInsideCharClass c DontIgnoreCase = [c]
caseInsideCharClass c IgnoreCase = if isAlpha c
                                   then (oppositeCase c) : [c]
                                   else [c]

caseOutsideCharClass :: Char -> MatchOptions -> String
caseOutsideCharClass c DontIgnoreCase = [c]
caseOutsideCharClass c opt = if shouldBeInBrackets
                             then '[' : cased ++ "]"
                             else cased
  where cased = caseInsideCharClass c opt
        shouldBeInBrackets = (length cased) > 1

-- ghci> :l 8_a_2.hs
-- [1 of 1] Compiling Main             ( 8_a_2.hs, interpreted )
-- Ok, one module loaded.

-- ghci> globToRegex "f[o-1].c" IgnoreCase
-- "^[Ff][O-za-1o-ZA-*** Exception: 1 is not a caseable character
-- CallStack (from HasCallStack):
--   error, called at 8_a_2.hs:48:23 in main:Main

-- ghci> globToRegex "f[gH,i-kL-N]*.c" DontIgnoreCase
-- "^f[gH,i-kL-N].*\\.c$"
-- ghci> matchesGlob "fhjM.c" "f[gH,i-kL-N]*.c" DontIgnoreCase
-- False
-- ghci> matchesGlob "fHjM.c" "f[gH,i-kL-N]*.c" DontIgnoreCase
-- True

-- ghci> globToRegex "f[gH,i-kL-N]*.c" IgnoreCase
-- "^[Ff][GghH,i-kI-KL-Nl-n].*\\.[Cc]$"
-- ghci> matchesGlob "fhjM.c" "f[gH,i-kL-N]*.c" IgnoreCase
-- True
-- ghci> matchesGlob "fGhJm.c" "f[gH,i-kL-N]*.c" IgnoreCase
-- True

-- ghci> globToRegex "f[W-d]*.c" DontIgnoreCase
-- "^f[W-d].*\\.c$"
-- ghci> matchesGlob "fxa.c" "f[W-d]*.c" DontIgnoreCase
-- False
-- ghci> matchesGlob "fXa.c" "f[W-d]*.c" DontIgnoreCase
-- True

-- ghci> globToRegex "f[W-d]*.c" IgnoreCase
-- "^[Ff][w-za-dW-ZA-D].*\\.[Cc]$"
-- ghci> matchesGlob "fwa.c" "f[W-d]*.c" IgnoreCase
-- True
-- ghci> matchesGlob "fXawD.c" "f[W-d]*.c" IgnoreCase
-- True
