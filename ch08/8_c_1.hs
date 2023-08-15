-- Write a version of globToRegex that uses the type signature earlier.

{-- From examples/examples/ch08/GlobRegex.hs modified according to the assignment --}
import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String

globToRegex cs = propagateError regex
  where regex = globToRegex' cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right ('^' : s ++ "$")

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = propagateError regex
  where regex = globToRegex' cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right (".*" ++ s)

globToRegex' ('?':cs) = propagateError regex
  where regex = globToRegex' cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right ("." ++ s)

globToRegex' ('[':'!':c:cs) = propagateError charCls
  where charCls = charClass cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right ("[^" ++ c : s)

globToRegex' ('[':c:cs) = propagateError charCls
  where charCls = charClass cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right ('['  :  c : s)

globToRegex' ('[':_) = Left "unterminated character class"

globToRegex' (c:cs) = propagateError regex
  where regex = globToRegex' cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right ((escape c) ++ s)


escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"


charClass :: String -> Either GlobError String

charClass (']':cs) = propagateError regex
  where regex = globToRegex' cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right (']' : s)

charClass (c:cs) = propagateError charCls
  where charCls = charClass cs
        propagateError (Left e) = Left e
        propagateError (Right s) = Right (c : s)

charClass [] = Left "unterminated character class"
{-- End of code from examples --}


-- ghci> :l 8_c_1.hs
-- [1 of 1] Compiling Main             ( 8_c_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> globToRegex "["
-- Left "unterminated character class"

-- ghci> globToRegex "[]"
-- Left "unterminated character class"

-- ghci> globToRegex "a?b*c[!DE][FG]+"
-- Right "^a.b.*c[^DE][FG]\\+$"
