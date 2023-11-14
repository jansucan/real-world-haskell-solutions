-- Although the file finding code we described in this chapter is a good vehicle
-- for learning, it's not ideal for real systems programming tasks, because
-- Haskell's portable I/O libraries don't expose enough information to let us
-- write interesting and complicated queries.
--
-- Port the code from this chapter to your platform's native API, either
-- System.Posix or System.Win32.


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
import System.Posix.Types (EpochTime, FileOffset, FileMode)
import System.Posix.Files (getFileStatus, fileSize, modificationTime, fileMode, isDirectory)


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

atMostThreePictures :: Iterator [FilePath]

atMostThreePictures paths info
    | length paths == 3
      = Done paths
    | infoIsDirectory info && takeFileName path == ".svn"
      = Skip paths
    | extension `elem` [".jpg", ".png"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info
{-- End of code from examples --}


{-- From examples/examples/ch09/ControlledVisit.hs modified according to the assignment --}
data Info = Info {
      infoPath :: FilePath
    , infoIsDirectory :: Bool
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
  perms <- maybeIO (return (fileMode fileStatus))
  size <- maybeIO (return (fileSize fileStatus))
  modified <- maybeIO (return (modificationTime fileStatus))
  return (Info path isDir perms size modified)


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


allFiles :: Iterator [FilePath]
allFiles paths info = Continue ((infoPath info) : paths)

-- Contents of the test directory (reused from the 9_c_3 exercise):
-- test-9_c_3/
-- test-9_c_3/e.jpg
-- test-9_c_3/dirA
-- test-9_c_3/dirA/b.jpg
-- test-9_c_3/dirA/a.png
-- test-9_c_3/.svn
-- test-9_c_3/.svn/svn.jpg
-- test-9_c_3/dirB
-- test-9_c_3/dirB/d.png
-- test-9_c_3/dirB/c.jpg

-- ghci> :l 9_d_1.hs
-- [1 of 1] Compiling Main             ( 9_d_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> foldTree atMostThreePictures [] "test-9_c_3/"
-- ["test-9_c_3/dirA/a.png","test-9_c_3/dirA/b.jpg","test-9_c_3/e.jpg"]

-- ghci> foldTree allFiles [] "test-9_c_3/"
-- ["test-9_c_3/dirB/c.jpg",
--  "test-9_c_3/dirB/d.png",
--  "test-9_c_3/dirB",
--  "test-9_c_3/.svn/svn.jpg",
--  "test-9_c_3/.svn",
--  "test-9_c_3/dirA/a.png",
--  "test-9_c_3/dirA/b.jpg",
--  "test-9_c_3/dirA",
--  "test-9_c_3/e.jpg"]
