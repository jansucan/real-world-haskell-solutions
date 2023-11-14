-- Add the ability to find out who owns a directory entry to your code. Make
-- this information available to predicates.

-- For simpler implementation, numeric user IDs are used instead of user names.

{-- From examples/examples/ch09/FoldDir.hs modified according to the assignment
 --
 -- Added missing import of takeBaseName.
 -- Fixed appending correct root path to directory entries when constructing
 -- their full paths.
 --}
import Control.Exception
import Control.Monad (liftM)
import Data.Char (toLower)

import System.FilePath ((</>), takeFileName, takeExtension, takeBaseName)
import System.Posix.Directory (DirStream, openDirStream, readDirStream, closeDirStream)
import System.Posix.Types (EpochTime, FileOffset, FileMode, UserID)
import System.Posix.Files (getFileStatus, fileSize, modificationTime, fileMode, isDirectory,
                           fileOwner)


data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed


foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed subpath

    walk seed root (name:names) = do
      let path' = root </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' root names
        Continue seed'
          | infoIsDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') root names
          | otherwise -> walk seed' root names
    walk seed _ _ = return (Continue seed)
{-- End of code from examples --}


{-- From examples/examples/ch09/ControlledVisit.hs modified according to the assignment --}
data Info = Info {
      infoPath :: FilePath
    , infoIsDirectory :: Bool
    , infoOwner :: UserID
    , infoPerms :: Maybe FileMode
    , infoSize :: Maybe FileOffset
    , infoModTime :: Maybe EpochTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(SomeException _) -> return Nothing) (Just `liftM` act)

getInfo path = do
  fileStatus <- getFileStatus path
  let isDir = isDirectory fileStatus
  let owner = fileOwner fileStatus
  perms <- maybeIO (return (fileMode fileStatus))
  size <- maybeIO (return (fileSize fileStatus))
  modified <- maybeIO (return (modificationTime fileStatus))
  return (Info path isDir owner perms size modified)


getDirContents :: FilePath -> IO [String]
getDirContents path = do
  entries <- bracket (openDirStream path) closeDirStream (readStream [])
  return entries
  where
    readStream :: [String] -> DirStream -> IO [String]
    readStream entries ds = do
      e <- readDirStream ds
      if e == ""
        then return entries
        else readStream (entries ++ [e]) ds


getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- handle (\(SomeException _) -> return []) (getDirContents path)
    return (filter (`notElem` [".", ".."]) names)
{-- End of code from examples --}


filesOwnedBy :: UserID -> Iterator [FilePath]
filesOwnedBy userId paths info
    | owner == userId
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where owner = infoOwner info
        path = infoPath info


-- Contents of the test directory containing files owned by users jan (user ID
-- 1000), root (user ID 0), and nobody (user ID 99). The directories are owned
-- by 'jan':
--
-- test-9_d_2/
-- test-9_d_2/dirA
-- test-9_d_2/dirA/root
-- test-9_d_2/dirA/jan
-- test-9_d_2/dirA/root2
-- test-9_d_2/dirB
-- test-9_d_2/dirB/nobody2
-- test-9_d_2/dirB/root
-- test-9_d_2/dirB/nobody
-- test-9_d_2/root
-- test-9_d_2/jan
-- test-9_d_2/nobody

-- ghci> :l 9_d_2.hs
-- [1 of 1] Compiling Main             ( 9_d_2.hs, interpreted )
-- Ok, one module loaded.

-- ghci>  foldTree (filesOwnedBy 1000) [] "test-9_d_2"
-- ["test-9_d_2/jan","test-9_d_2/dirB","test-9_d_2/dirA/jan","test-9_d_2/dirA"]

-- ghci>  foldTree (filesOwnedBy 0) [] "test-9_d_2"
-- ["test-9_d_2/root","test-9_d_2/dirB/root","test-9_d_2/dirA/root2","test-9_d_2/dirA/root"]

-- ghci>  foldTree (filesOwnedBy 99) [] "test-9_d_2"
-- ["test-9_d_2/nobody","test-9_d_2/dirB/nobody","test-9_d_2/dirB/nobody2"]
