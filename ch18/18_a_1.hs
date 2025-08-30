-- Modify the App type synonym to swap the order of ReaderT and WriterT. What
-- effect does this have on the runApp execution function?


-- Typo in the assignment: There should be StateT instead of WriterT. WriterT is
-- added in the next exercise.


-- In the execution function, the runStateT and runReaderT functions have to be
-- swapped as well.


{-- From examples/examples/ch18/UglyStack.hs and modified --}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import CountEntries (listDirectory)

-- System.Directory also has listDirectory function. Hide it to not conflict
-- with our version from CountEntries module.
import System.Directory hiding (listDirectory)
import System.FilePath
import Control.Monad -- Needed for the forM and 'when'
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)


type App = StateT AppState (ReaderT AppConfig IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runReaderT (runStateT k state) config

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest
{-- End of code from examples --}


-- ghci> :l 18_a_1.hs
-- [1 of 3] Compiling CountEntries     ( CountEntries.hs, interpreted )
-- [2 of 3] Compiling Main             ( 18_a_1.hs, interpreted )
-- Ok, two modules loaded.

-- ghci> runApp (constrainedCount 0 "../ch08") 2
-- ([("../ch08",12),("../ch08/test-8_b_3",3),("../ch08/test-8_b_3/dir1",7),("../ch08/test-8_b_3/dir2",1)],AppState {stDeepestReached = 2})
