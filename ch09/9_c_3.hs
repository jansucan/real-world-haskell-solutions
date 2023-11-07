-- Write a combinator library that makes it possible to express the kinds of
-- iterators that foldTree accepts. Does it make the iterators you write any
-- more succinct?

-- From the assignment it's not clear how the combinators for the iterators
-- should work.
--
-- The operators from the previous exercises in this chapter (>, <, ==, &&, ||)
-- cannot be directly transferred here because this iterator returns three
-- values (Done, Skip, Continue) instead just two (True, False). We would have
-- to define three-value logic system for the Iterate type.
--
-- Let's just define a simple (A `andIter` B) combinator for chaining the
-- iterators as:
--   1. Evaluate (A seed info) to resultA
--   2. If the resultA is Skip or Done, then return the result
--   3. Else if the resultA is (Continue resultSeed) and (resultSeed /= seed),
--      then return the resultA
--   4. Return the result of evaluating (B seed info)


{-- From examples/examples/ch09/FoldDir.hs modified according to the assignment
 --
 -- Added missing import of takeBaseName.
 -- Fixed appending correct root path to directory entries when constructing
 -- their full paths.
 --}
import ControlledVisit (Info(..), getInfo, getUsefulContents, isDirectory)

import Control.Monad (liftM)
import Data.Char (toLower)
import System.FilePath ((</>), takeFileName, takeExtension, takeBaseName)

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
          | isDirectory info -> do
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
    | isDirectory info && takeFileName path == ".svn"
      = Skip paths
    | extension `elem` [".jpg", ".png"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info
{-- End of code from examples --}


andIter :: (Eq a) => Iterator a -> Iterator a -> Iterator a
andIter a b seed info = case resA of
                          Done _                  -> resA
                          Skip _                  -> resA
                          cont@(Continue newSeed) -> if newSeed /= seed
                                                     then cont
                                                     else resB
  where resA = a seed info
        resB = b seed info

threePaths :: Iterator [FilePath]
threePaths paths info
    | length paths == 3
      = Done paths
    | otherwise
      = Continue paths
  where path = infoPath info

skipSvn :: Iterator [FilePath]
skipSvn paths info
    | isDirectory info && takeFileName path == ".svn"
      = Skip paths
    | otherwise
      = Continue paths
  where path = infoPath info

includePictures :: Iterator [FilePath]
includePictures paths info
    | extension `elem` [".jpg", ".png"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info

myThreePictures :: Iterator [FilePath]
myThreePictures = (threePaths `andIter` skipSvn) `andIter` includePictures


-- Contents of the test directory:
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

-- ghci> :l 9_c_3.hs
-- [1 of 2] Compiling ControlledVisit  ( ControlledVisit.hs, interpreted )
-- [2 of 2] Compiling Main             ( 9_c_3.hs, interpreted )
-- Ok, two modules loaded.

-- ghci> foldTree atMostThreePictures [] "test-9_c_3/"
-- ["test-9_c_3/dirA/a.png","test-9_c_3/dirA/b.jpg","test-9_c_3/e.jpg"]

-- ghci> foldTree myThreePictures [] "test-9_c_3/"
-- ["test-9_c_3/dirA/a.png","test-9_c_3/dirA/b.jpg","test-9_c_3/e.jpg"]


-- It doesn't make the iterators more succinct.
--
-- The compound iterator (e.g., myThreePictures) is more readable thanks to
-- hiding the individual iterators in well-named functions, but because the
-- individual iterators cannot share common parts (the 'where' clause) the code
-- duplication cancels the positives.
