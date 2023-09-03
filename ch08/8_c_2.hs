-- Modify the type signature of namesMatching so that it encodes the possibility
-- of a bad pattern, and make it use your rewritten globToRegex function.

{-- From examples/examples/ch08/Glob.hs modified according to the assignment --}
 --
 -- An error in the listMatches function has been fixed as described in the
 -- solution of 8_a_2.hs.
 --}
import Module_8_c_1 (GlobError, globToRegex)

import Text.Regex.Posix ((=~))

import Control.Exception
import Control.Monad (forM)

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)

import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

{-- From examples/examples/ch08/GlobRegex.hs modified according to the assignment --}
matchesGlob :: FilePath -> String -> Either GlobError Bool
name `matchesGlob` pat = propagateError regex
  where regex = globToRegex pat
        propagateError (Left e) = Left e
        propagateError (Right s) = Right (name =~ s)
{-- End of code from examples --}

namesMatching :: String -> IO (Either GlobError [FilePath])

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (Right (if exists then [pat] else []))

  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return (Right [dirName])
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          propagateErrorDirs dirs listDir baseName
      where
        propagateErrorDirs (Left e) _ _ = return (Left e)
        propagateErrorDirs (Right dirs) listDir baseName =
          propagateErrorListDirs dirs listDir baseName

        propagateErrorListDirs [] _ _ = return (Right [])
        propagateErrorListDirs (d:ds) listDir baseName = do
          baseNames <- listDir d baseName
          rest <- propagateErrorListDirs ds listDir baseName
          propagateError d baseNames rest
          where propagateError _ (Left e) _ = return (Left e)
                propagateError _ _ (Left e) = return (Left e)
                propagateError dir (Right baseNames) (Right rest)= do
                  let dirBaseNames = map (dir </>) baseNames
                  return (Right (dirBaseNames ++ rest))

listMatches :: FilePath -> String -> IO (Either GlobError [String])
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (\(SomeException _) -> return (Right [])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filterGlobError names' pat)

filterGlobError :: [String] -> String -> Either GlobError [String]
filterGlobError [] _ = Right []
filterGlobError (n:ns) pat = propagateErrorConcat match filteredRest
  where match = matchesGlob n pat
        filteredRest = filterGlobError ns pat
        propagateErrorConcat (Left e) _ = Left e
        propagateErrorConcat _ (Left e) = Left e
        propagateErrorConcat (Right matches) (Right rs) = Right (if matches then n:rs else rs)

isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO (Either GlobError [String])
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (Right (if exists then [baseName] else []))

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
{-- End of code from examples --}


-- ghci> :l 8_c_2.hs
-- [1 of 2] Compiling Module_8_c_1     ( Module_8_c_1.hs, interpreted )
-- [2 of 2] Compiling Main             ( 8_c_2.hs, interpreted )
-- Ok, two modules loaded.

-- ghci> namesMatching "*.hs"
-- Right ["./8_b_3.hs","./8_a_2.hs","./8_a_1.hs","./GlobRegex.hs","./8_c_2.hs","./Module_8_c_1.hs"]

-- ghci> namesMatching "*/[d]*/*.c"
-- Right ["./test-8_b_3/dir1/A.c","./test-8_b_3/dir1/B.c"]

-- ghci> namesMatching "*/[/*.c"
-- Left "unterminated character class"

-- ghci> namesMatching "*/[]/*.c"
-- Left "unterminated character class"
