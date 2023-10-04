-- Modify foldTree to allow the caller to change the order of traversal of
-- entries in a directory.

{-- From examples/examples/ch09/FoldDir.hs modified according to the assignment
 --
 -- Added missing import of takeBaseName.
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

foldTree :: Iterator a -> a -> FilePath -> ([FilePath] -> IO [FilePath]) -> IO a

foldTree iter initSeed path order = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= order >>= walk seed

    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)
{-- End of code from examples --}

orderId :: [FilePath] -> IO [FilePath]
orderId fs = return fs

orderReverse :: [FilePath] -> IO [FilePath]
orderReverse fs = return (reverse fs)

-- Helper iterator that returns the traversed files for easy observing of
-- effects of the order functions.
allPaths :: Iterator [FilePath]
allPaths paths info = Continue ((infoPath info) : paths)


-- ghci> :l 9_c_1.hs
-- [1 of 2] Compiling ControlledVisit  ( ControlledVisit.hs, interpreted )
-- [2 of 2] Compiling Main             ( 9_c_1.hs, interpreted )
-- Ok, two modules loaded.

-- ghci> foldTree allPaths [] "test-9_b_4/" orderId
-- ["test-9_b_4/C.c","test-9_b_4/E.hs","test-9_b_4/D.hs","test-9_b_4/B.c","test-9_b_4/A.c","test-9_b_4/F.hs"]

-- ghci> foldTree allPaths [] "test-9_b_4/" orderReverse
-- ["test-9_b_4/F.hs","test-9_b_4/A.c","test-9_b_4/B.c","test-9_b_4/D.hs","test-9_b_4/E.hs","test-9_b_4/C.c"]
