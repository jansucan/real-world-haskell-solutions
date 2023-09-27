-- Write a wrapper for 'traverse' that lets you control traversal using one
-- predicate and filter results using another.

-- From the assignment it's not completely clear what controlling traversal
-- means.
--
-- Let's assume that it means visiting files for which the control predicate is
-- true first.

import ControlledVisit (traverse', Info(..))
-- Use predicates for the new Info type from the previous exercise
import Module_9_b_3

import Data.List
import System.FilePath (takeExtension)

traverseWrapper :: (Info -> Bool) -> (Info -> Bool) -> FilePath -> IO [Info]
traverseWrapper orderP filterP path = do
  res <- traverse' (order orderP) path
  return (filterResults filterP res)

compareInfos :: (Info -> Bool) -> Info -> Info -> Ordering
compareInfos p infoA infoB = if a && (not b)
                             then LT -- "greater" Info should appear in the sorted list first
                             else if (not a) && b
                                  then GT
                                  else EQ
  where a = p infoA
        b = p infoB

order :: (Info -> Bool) -> ([Info] -> [Info])
order p = sortBy (compareInfos p)

filterResults :: (Info -> Bool) -> [Info] -> [Info]
filterResults p infos = filter p infos


-- > ls -l
-- total 24
-- -rw-r--r-- 1 jan users 1 Sep 27 15:28 A.c
-- -rw-r--r-- 1 jan users 2 Sep 27 15:28 B.c
-- -rw-r--r-- 1 jan users 3 Sep 27 15:28 C.c
-- -rw-r--r-- 1 jan users 4 Sep 27 15:28 D.hs
-- -rw-r--r-- 1 jan users 5 Sep 27 15:28 E.hs
-- -rw-r--r-- 1 jan users 6 Sep 27 15:28 F.hs

idTest :: Info -> Bool
idTest _ = True

-- ghci> :l 9_b_4.hs
-- [1 of 3] Compiling ControlledVisit  ( ControlledVisit.hs, interpreted )
-- [2 of 3] Compiling Module_9_b_3     ( Module_9_b_3.hs, interpreted )
-- [3 of 3] Compiling Main             ( 9_b_4.hs, interpreted )
-- Ok, three modules loaded.

sizeTest1 = infoSize `lesserP` (Just 3)

sizeTest2 = infoSize `greaterP` (Just 2)

nameTest = (liftPath takeExtension) `equalP` ".c"

complexTest1 = (infoSize `lesserP` (Just 4)) `andP` ((liftPath takeExtension) `equalP` ".hs")

complexTest2 = (infoSize `greaterP` (Just 4)) `andP` (infoSize `lesserP` (Just 6))
               `andP` ((liftPath takeExtension) `equalP` ".hs")

-- ghci> traverseWrapper idTest idTest "test-9_b_4/"
-- [Info {infoPath = "test-9_b_4/", ..., infoSize = Nothing, ...},
--  Info {infoPath = "test-9_b_4/F.hs", ..., infoSize = Just 6, ...},
--  Info {infoPath = "test-9_b_4/A.c", ..., infoSize = Just 1, ...},
--  Info {infoPath = "test-9_b_4/B.c", ..., infoSize = Just 2, ...},
--  Info {infoPath = "test-9_b_4/D.hs", ..., infoSize = Just 4, ...},
--  Info {infoPath = "test-9_b_4/E.hs", ..., infoSize = Just 5, ...},
--  Info {infoPath = "test-9_b_4/C.c", ..., infoSize = Just 3, ...}]

-- ghci> traverseWrapper sizeTest1 idTest "test-9_b_4/"
-- [Info {infoPath = "test-9_b_4/", ..., infoSize = Nothing, ...},
--  Info {infoPath = "test-9_b_4/A.c", ..., infoSize = Just 1, ...},
--  Info {infoPath = "test-9_b_4/B.c", ..., infoSize = Just 2, ...},
--  Info {infoPath = "test-9_b_4/F.hs", ..., infoSize = Just 6, ...},
--  Info {infoPath = "test-9_b_4/D.hs", ..., infoSize = Just 4, ...},
--  Info {infoPath = "test-9_b_4/E.hs", ..., infoSize = Just 5, ...},
--  Info {infoPath = "test-9_b_4/C.c", ..., infoSize = Just 3, ...}]

-- ghci> traverseWrapper sizeTest2 nameTest "test-9_b_4/"
-- [Info {infoPath = "test-9_b_4/C.c", ..., infoSize = Just 3, ...},
--  Info {infoPath = "test-9_b_4/A.c", ..., infoSize = Just 1, ...},
--  Info {infoPath = "test-9_b_4/B.c", ..., infoSize = Just 2, ...}]

-- ghci> traverseWrapper idTest complexTest1 "test-9_b_4/"
-- []

-- ghci> traverseWrapper idTest complexTest2 "test-9_b_4/"
-- [Info {infoPath = "test-9_b_4/E.hs", ..., infoSize = Just 5, ...}]
