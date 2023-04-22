-- Use ghci to explore what happens if you pass a malformed pattern, such as [,
-- to globToRegex. Write a small function that calls globToRegex, and pass it a
-- malformed pattern. What happens?

-- When trying to load the GlobRegex.hs file into ghci, I got an error about
-- that the module was not found. I used Hoogle (https://hoogle.haskell.org/) to
-- search for Text.Regex.Posix module and found it was in regex-posix package. I
-- use stack, so I installed the package by
--
--   stack install regex-posix

import GlobRegex

callGlobToRegex glob = globToRegex glob

-- ghci> :l 8_a_1.hs
-- [1 of 2] Compiling GlobRegex        ( GlobRegex.hs, interpreted )
-- [2 of 2] Compiling Main             ( 8_a_1.hs, interpreted )
-- Ok, two modules loaded.
-- ghci> callGlobToRegex "["
-- "^*** Exception: unterminated character class
-- CallStack (from HasCallStack):
--   error, called at ./GlobRegex.hs:27:31 in main:GlobRegex
