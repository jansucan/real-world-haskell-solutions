-- The foldTree function performs preorder traversal. Modify it to allow the
-- caller to determine the order of traversal.

-- Preorder means that the iterator is applied to (the into of) a directory before
-- it is applied to its children.
-- Postorder means that the iterator is applied to (the into of) a directory after
-- it is applied to its children.

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

data TraversalOrder = Preorder | Postorder deriving (Eq)

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> TraversalOrder -> IO a

foldTree iter initSeed path traversalOrder = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    walk = if traversalOrder == Preorder
           then walkPreorder
           else walkPostorder
    fold seed subpath = getUsefulContents subpath >>= walk seed subpath

    walkPreorder seed root (name:names) = do
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
    walkPreorder seed _ _ = return (Continue seed)
{-- End of code from examples --}

    walkPostorder seed root (name:names) = do
      let path' = root </> name
      info <- getInfo path'
      seed' <- if isDirectory info
               then fold seed path'
               else return (Continue seed)
      case seed' of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' root names
        Continue seed' -> do
          let x = iter seed' info
          case x of
            done@(Done _) -> return done
            seed''        -> walk (unwrap seed'') root names
    walkPostorder seed _ _ = return (Continue seed)

-- Helper iterator that returns the traversed files for easy observing of
-- effects of the order functions.
allPaths :: Iterator [FilePath]
allPaths paths info = Continue (paths ++ [(infoPath info)])


-- ghci> :l 9_c_2.hs
-- [1 of 2] Compiling ControlledVisit  ( ControlledVisit.hs, interpreted )
-- [2 of 2] Compiling Main             ( 9_c_2.hs, interpreted )
-- Ok, two modules loaded.

-- Output of the following commands is manually formatted for clarity
-- ghci> foldTree allPaths [] "test-9_b_1/" Preorder
-- ["test-9_b_1/dirC",
--  "test-9_b_1/dirC/F",
--  "test-9_b_1/dirC/E",
--  "test-9_b_1/A",
--  "test-9_b_1/B",
--  "test-9_b_1/dirD",
--  "test-9_b_1/dirD/G",
--  "test-9_b_1/dirD/H"]

-- ghci> foldTree allPaths [] "test-9_b_1/" Postorder
-- ["test-9_b_1/dirC/F"
--  "test-9_b_1/dirC/E"
--  "test-9_b_1/dirC"
--  "test-9_b_1/A"
--  "test-9_b_1/B"
--  "test-9_b_1/dirD/G"
--  "test-9_b_1/dirD/H"
--  "test-9_b_1/dirD"]
