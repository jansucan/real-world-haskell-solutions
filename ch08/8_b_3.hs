-- The * wild card only matches names within a single directory. Many shells
-- have an extended wild card syntax, **, that matches names recursively in all
-- directories. For example, **.c would mean “match a name ending in .c in this
-- directory or any subdirectory at any depth”. Implement matching on **
-- wildcards.

-- The assignment seems incorrect. Or maybe it was correct at the time of
-- writing the book. I used bash and Python's glob for checking this
-- functionality (globstar). From bash manual page:
--
--   "..., two adjacent *s used as a single pattern will match all files and
--   zero or more directories and subdirectories. If followed by a /, two
--   adjacent *s will match only directories and subdirectories."
--
-- According to those, patterns like '**.c' don't match files recursively. That
-- behavior can be achieved by patterns like '**/*.c'.
--
-- To follow the assignment regardless, I implemented what I call an extended
-- globstar: If a pattern in any component of a file path contains '**' among
-- other characters, each '**' sequence is converted to a single '*'. This new
-- pattern is used for matching files and directories in a current directory. In
-- matching directories, the searching continues with the tail of the file path,
-- and in all directories (including the matching ones) the searching continues
-- with the whole file path.


{-- From examples/examples/ch08/Glob.hs modified according to the assignment --}
import Data.List (sort, group)

import Control.Monad (forM)
import GlobRegex (matchesGlob)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)

import System.FilePath ((</>), splitPath)


find :: String -> IO [FilePath]
find path
  | not (isPattern path) = do
      exists <- doesNameExist path
      return (if exists then [path] else [])
  | otherwise = do
      (rootDir, rest) <- splitRootDir path
      found' <- findPattern rootDir "" rest
      -- Keep only one match of any single file.
      --
      -- In this implementation of the globstar matching, some matches can be
      -- present multiple times in the result. For example, for file C/B/A and
      -- pattern '**/A', the file can be found both as 'A' in directory C/B and
      -- '*/A' in directory C.
      let found = map head ((group.sort) found')
      return found

splitRootDir :: FilePath -> IO (FilePath, [FilePath])
splitRootDir path = do
  currentDir <- getCurrentDirectory
  let split = splitPath path
  let first = head split
  return (if first == "/"
          then (first, (tail split))
          else (currentDir, split))

-- Parameters:
--   rootDir      - the root directory where the searching started
--   matchingRoot - part of the search path/pattern that matches. The path for
--                  listing directory content is rootDir/matchingRoot.
--   path         - the remaining part of the search path/pattern to be matched.
findPattern :: FilePath -> FilePath -> [String] -> IO [FilePath]
findPattern rootDir matchingRoot [] = do
  return []
findPattern rootDir matchingRoot path = do
  let pat = head path
  let rest = tail path
  let isGlobstar = isPatternGlobstar pat
  let dirOnly = ((last pat) == '/')
  let hidden = ((head pat) == '.')
  let patToMatch = getPatternForMatching pat

  -- List regular files and directories separately for matching the current
  -- search path/pattern component
  files <- listDir (rootDir </> matchingRoot) doesFileExist hidden
  dirs <- listDir (rootDir </> matchingRoot) doesDirectoryExist hidden

  -- For recursing in the case of globstar pattern, get all directories
  -- regardless of whether they are hidden or not
  globstarDirs <- listDir (rootDir </> matchingRoot) doesDirectoryExist False
  let filesMatch = if not dirOnly
                   then filter (`matchesGlob` patToMatch) files
                   else []
  let dirsMatch = filter (`matchesGlob` patToMatch) dirs
  let allMatch = filesMatch ++ dirsMatch

  -- For globstar, search all directories again with the same 'path' regardless
  -- of whether they match or not
  globstarFound <- if isGlobstar
                   then do
                     namesRecursive <- forM globstarDirs $ \dir -> do
                       findPattern rootDir (matchingRoot </> dir) path
                     -- Globstar also means no directory. Search directly in the
                     -- matching root.
                     namesCurrent <- findPattern rootDir matchingRoot rest
                     return ((concat namesRecursive) ++ namesCurrent)
                   else return []
  found <- if null rest
         -- This is the last component of the search path. Return matches.
         then do
           return (map ((</>) matchingRoot) allMatch)
         -- Not the last component. Recurse into matching directories.
         else do
           names <- forM dirsMatch $ \dir -> do
             findPattern rootDir (matchingRoot </> dir) rest
           return (concat names)

  -- The overall result consists of the normal matches and the globstar
  -- recursive matches
  return (found ++ globstarFound)

listDir :: FilePath -> (FilePath -> IO Bool) -> Bool -> IO [FilePath]
listDir rootDir existsFunc hidden = do
  dirContents <- getDirectoryContents rootDir
  files <- forM dirContents $ \file -> do
    let path = (</>) rootDir file
    exists <- existsFunc path
    if exists
      then return file
      else return ""
  -- Keep only files of wanted type
  let filesWanted = filter (\f -> f /= "") files
  let filesHidden = if hidden
                    then filter (\f -> isHidden f && f /= "." && f /= "..") filesWanted
                    else filter (not . isHidden) filesWanted
  return filesHidden

getPatternForMatching :: String -> String
getPatternForMatching ('*':'*':rest)  = '*':(getPatternForMatching rest)
getPatternForMatching ('/':rest)      = getPatternForMatching rest
getPatternForMatching (p:rest)        = p:(getPatternForMatching rest)
getPatternForMatching  p              = p

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

isPatternGlobstar :: String -> Bool
isPatternGlobstar ('*':'*':_)  = True
isPatternGlobstar (_:rest)     = isPatternGlobstar rest
isPatternGlobstar _            = False

isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
{-- End of code from examples --}


-- The testing directory structure is
--
-- test-8_b_3/
-- |-- A.c
-- |-- dir1
-- |   |-- .hiddenC.c
-- |   |-- .hiddenD.c
-- |   |-- .hidden_dirG_c
-- |   |   `-- A.c
-- |   |-- A.c
-- |   |-- B.c
-- |   |-- dirE_c
-- |   |   `-- file
-- |   `-- dirF_c
-- |       `-- file
-- `-- dir2
--     `-- dir1
--         `-- dir3
--             |-- .hiddenC.c
--             |-- .hiddenD.c
--             |-- .hidden_dirG_c
--             |   `-- A.c
--             |-- A.c
--             |-- B.c
--             |-- dirE_c
--             |   `-- file
--             `-- dirF_c
--                 `-- file
--
-- 10 directories, 15 files


-- ghci> :l 8_b_3.hs
-- [1 of 2] Compiling GlobRegex        ( GlobRegex.hs, interpreted )
-- [2 of 2] Compiling Main             ( 8_b_3.hs, interpreted )
-- Ok, two modules loaded.

-- ghci> find "test-8_b_3/dir1/nonexistent.c"
-- []

-- Not a pattern
-- ghci> find "test-8_b_3/dir1/A.c"
-- ["test-8_b_3/dir1/A.c"]


-- Normal pattern for files and directories
-- ghci> find "test-8_b_3/dir1/*c"
-- ["test-8_b_3/dir1/A.c","test-8_b_3/dir1/B.c","test-8_b_3/dir1/dirE_c","test-8_b_3/dir1/dirF_c"]

-- Normal pattern for hidden files and directories
-- ghci> find "test-8_b_3/dir1/.*c"
-- ["test-8_b_3/dir1/.hiddenD.c","test-8_b_3/dir1/.hiddenC.c","test-8_b_3/dir1/.hidden_dirG_c"]

-- Normal pattern for directories only
-- ghci> find "test-8_b_3/dir1/*c/"
-- ["test-8_b_3/dir1/dirE_c","test-8_b_3/dir1/dirF_c"]


-- Globstar also means 'zero directories' (searching in current directory)
-- ghci> find "test-8_b_3/dir1/**/*.c"
-- ["test-8_b_3/dir1/A.c","test-8_b_3/dir1/B.c"]

-- Globstar for files and directories
-- ghci> find "test-8_b_3/**c"
-- ["test-8_b_3/A.c","test-8_b_3/dir2/dir1/dir3/A.c","test-8_b_3/dir2/dir1/dir3/B.c", \
--  "test-8_b_3/dir2/dir1/dir3/dirE_c","test-8_b_3/dir2/dir1/dir3/dirF_c","test-8_b_3/dir1/A.c", \
--  "test-8_b_3/dir1/B.c","test-8_b_3/dir1/dirE_c","test-8_b_3/dir1/dirF_c"]

-- Globstar for hidden files and directories
-- ghci> find "test-8_b_3/.**c"
-- ["test-8_b_3/dir2/dir1/dir3/.hiddenD.c","test-8_b_3/dir2/dir1/dir3/.hiddenC.c", \
--  "test-8_b_3/dir2/dir1/dir3/.hidden_dirG_c","test-8_b_3/dir1/.hiddenD.c", \
--  "test-8_b_3/dir1/.hiddenC.c","test-8_b_3/dir1/.hidden_dirG_c"]

-- Globstar for directories only
-- ghci> find "test-8_b_3/**c/"
-- ["test-8_b_3/dir2/dir1/dir3/dirE_c","test-8_b_3/dir2/dir1/dir3/dirF_c", \
--  "test-8_b_3/dir1/dirE_c","test-8_b_3/dir1/dirF_c"]

-- Globstar matching hidden directories in the path
-- Note that globstar also means 'zero directories'
-- ghci> find "test-8_b_3/.**/A.c"
-- ["test-8_b_3/dir2/dir1/dir3/.hidden_dirG_c/A.c","test-8_b_3/dir2/dir1/dir3/A.c", \
--  "test-8_b_3/dir1/.hidden_dirG_c/A.c","test-8_b_3/dir1/A.c","test-8_b_3/A.c"]

-- Globstar in multiple occurrences
-- ghci> find "test-8_b_3/**/dir1/**/A.c"
-- ["test-8_b_3/dir1/A.c","test-8_b_3/dir2/dir1/dir3/A.c"]

-- Globstar matching paths that are prefixes of other paths
-- ghci> find "test-8_b_3/**/dir1"
-- ["test-8_b_3/dir1","test-8_b_3/dir2/dir1"]
