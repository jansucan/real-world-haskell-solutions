-- 1. Add the WriterT transformer to the App monad transformer stack. Modify
--    runApp to work with this new setup.
--
-- 2. Rewrite the constrainedCount function to record results using the WriterT
--    transformer in your new App stack.


{-- From examples/examples/ch18/UglyStack.hs and modified --}
import CountEntries (listDirectory)

-- System.Directory also has listDirectory function. Hide it to not conflict
-- with our version from CountEntries module.
import System.Directory hiding (listDirectory)
import System.FilePath
import Control.Monad -- Needed for the forM_ and 'when'
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)


type App = WriterT [(FilePath, Int)] (ReaderT AppConfig (StateT AppState IO)) ()

runApp :: App -> Int -> IO (((), [(FilePath, Int)]), AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT (runWriterT k) config) state

constrainedCount :: Int -> FilePath -> App
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  cfg <- ask
  forM_ contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $
          put st { stDeepestReached = newDepth }
        constrainedCount newDepth newPath
      else return ()
{-- End of code from examples --}


-- ghci> :l 18_a_2.hs
-- [1 of 3] Compiling CountEntries     ( CountEntries.hs, interpreted )
-- [2 of 3] Compiling Main             ( 18_a_2.hs, interpreted )
-- Ok, two modules loaded.

-- ghci> runApp (constrainedCount 0 "../ch08") 2
-- (((),[("../ch08",12),("../ch08/test-8_b_3",3),("../ch08/test-8_b_3/dir1",7),("../ch08/test-8_b_3/dir2",1)]),AppState {stDeepestReached = 2})
