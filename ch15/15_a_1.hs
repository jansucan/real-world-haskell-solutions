-- 1. Using QuickCheck, write a test for an action in the MonadHandle monad, to
--    see if it tries to write to a file handle that is not open. Try it out on
--    safeHello.
--
-- 2. Write an action that tries to write to a file handle that it has
--    closed. Does your test catch this bug?


{-- From examples/examples/ch15/WriterIO.hs and modified --}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TypeSynonymInstances #-}

import Control.Monad.Writer
import MonadHandle
import System.IO (IOMode(..))
import SafeHello

import Test.QuickCheck

data Event = Close
           | Open
           | Write
           deriving (Show)

-- Added missing Functor and Applicative instance derivation
newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open] >> return path
    hPutStr h str = tell [Write]
    hClose h = tell [Close]
    hGetContents h = return ""
{-- End of code from examples --}


{-- From examples/examples/ch15/SafeHello.hs and modified --}
writeAfterCloseHello :: MonadHandle h m => FilePath -> m ()
writeAfterCloseHello path = do
  h <- openFile path WriteMode
  hClose h
  hPutStrLn h "hello world"
  hClose h
{-- End of code from examples --}


data State = Closed
           | Opened
           | None

-- Analyze the logged events for containing write of a closed file handle
hasWriteAfterClose :: [Event] -> Bool
hasWriteAfterClose es = hasWriteAfterClose' es None
  where
    hasWriteAfterClose' (Write:_) Closed = True
    hasWriteAfterClose' [] _ = False
    hasWriteAfterClose' (Close:es) state = hasWriteAfterClose' es Closed
    hasWriteAfterClose' (Open:es) state = hasWriteAfterClose' es Opened
    hasWriteAfterClose' (_:es) state = hasWriteAfterClose' es state



runSafeHello filePath = runWriterIO (safeHello filePath)
runWriteAfterCloseHello filePath = runWriterIO (writeAfterCloseHello filePath)

prop_noWriteAfterClose :: (FilePath -> ((), [Event])) -> FilePath -> Bool
prop_noWriteAfterClose action filePath = hasWriteAfterClose (snd (action filePath)) == False


-- ghci> :l 15_a_1.hs
-- [1 of 4] Compiling MonadHandle      ( MonadHandle.hs, interpreted )
-- [2 of 4] Compiling SafeHello        ( SafeHello.hs, interpreted )
-- [3 of 4] Compiling Main             ( 15_a_1.hs, interpreted )
-- Ok, three modules loaded.

-- ghci> quickCheck(prop_noWriteAfterClose runSafeHello)
-- +++ OK, passed 100 tests.

-- ghci> quickCheck(prop_noWriteAfterClose runWriteAfterCloseHello)
-- *** Failed! Falsified (after 1 test):
-- ""
