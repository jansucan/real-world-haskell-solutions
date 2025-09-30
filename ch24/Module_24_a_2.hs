-- Although we've already mentioned the existence of the strict-concurrency
-- package in the Hackage repository, try developing your own, as a wrapper
-- around the built-in MVar type. Following classic Haskell practice, make your
-- library type safe, so that users cannot accidentally mix uses of strict and
-- non-strict MVars.


-- The strict-concurrency package contains strict MVar and Chan. For simplicity,
-- I will
--   - implement only
--     - newEmptyMVar, newMVar, takeMVar, and putMVar
--     - newChan, writeChan, and readChan

--   - not mimic the strict-concurrency functions completely, to avoid dealing
--     with the NFData typeclass and normal forms
--
-- I assume that the classic Haskell practice means making sure that the users
-- cannot just simply change the import from the non-strict versions to strict
-- ones with the code still compiling.


module Module_24_a_2
    (
      StrictMVar
    , Module_24_a_2.newStrictEmptyMVar
    , Module_24_a_2.newStrictMVar
    , Module_24_a_2.takeMVar
    , Module_24_a_2.putMVar
    , StrictChan
    , Module_24_a_2.newStrictChan
    , Module_24_a_2.writeChan
    , Module_24_a_2.readChan
    ) where

import Control.Concurrent.MVar as NSM
import Control.Concurrent.Chan as NSC


newtype StrictMVar a = StrictM (MVar a)

unwrapStrictMVar :: StrictMVar a -> MVar a
unwrapStrictMVar (StrictM m) = m

newStrictEmptyMVar :: IO (StrictMVar a)
newStrictEmptyMVar = StrictM <$> NSM.newEmptyMVar

newStrictMVar :: a -> IO (StrictMVar a)
newStrictMVar val = val `seq` StrictM <$> NSM.newMVar val

takeMVar :: StrictMVar a -> IO a
takeMVar m = NSM.takeMVar $ unwrapStrictMVar m

putMVar :: StrictMVar a -> a -> IO ()
putMVar m val = val `seq` NSM.putMVar (unwrapStrictMVar m) val


newtype StrictChan a = StrictC (Chan a)

unwrapStrictChan :: StrictChan a -> Chan a
unwrapStrictChan (StrictC c) = c

newStrictChan :: IO (StrictChan a)
newStrictChan = StrictC <$> NSC.newChan

writeChan :: StrictChan a -> a -> IO ()
writeChan c val = val `seq` NSC.writeChan (unwrapStrictChan c) val

readChan :: StrictChan a -> IO a
readChan c = NSC.readChan (unwrapStrictChan c)


-- To test this module better (information hiding offered by the export
-- functionality), it is imported in the test_24_a_2.hs instead of loaded into
-- ghci. The test source file contains commented-out sections of code I will
-- uncomment individually (with uncommenting the relevant import statement) when
-- performing the tests.




-- Tests with non-strict versions (the Control.Concurrent module)

-- ghci> :l test_24_a_2.hs
-- [1 of 2] Compiling Main             ( test_24_a_2.hs, interpreted )
-- test_24_a_2.hs:26:8: error: [GHC-88464]
--     Variable not in scope: newStrictEmptyMVar :: IO (MVar a1)
--    |
-- 26 |   x <- newStrictEmptyMVar
--    |        ^^^^^^^^^^^^^^^^^^
--
-- test_24_a_2.hs:32:8: error: [GHC-88464]
--     Variable not in scope: newStrictChan :: IO (Chan a0)
--    |
-- 32 |   x <- newStrictChan
--    |        ^^^^^^^^^^^^^
--
-- Failed, unloaded all modules.


-- ghci> :l test_24_a_2.hs
-- [1 of 2] Compiling Main             ( test_24_a_2.hs, interpreted )
-- Ok, one module loaded.
-- ghci> m1NonStrict
-- ghci> m2NonStrict
-- ghci> cNonStrict




-- Tests with strict versions (this module)

-- ghci> :l test_24_a_2.hs
-- [1 of 3] Compiling Module_24_a_2    ( Module_24_a_2.hs, interpreted )
-- [2 of 3] Compiling Main             ( test_24_a_2.hs, interpreted )
-- Ok, two modules loaded.
-- ghci> mTypeConstructorAccessible
-- ghci> cTypeConstructorAccessible


-- ghci> :l test_24_a_2.hs
-- [1 of 3] Compiling Module_24_a_2    ( Module_24_a_2.hs, interpreted )
-- [2 of 3] Compiling Main             ( test_24_a_2.hs, interpreted )
-- test_24_a_2.hs:16:9: error: [GHC-76037]
--     Not in scope: data constructor ‘StrictM’
--    |
-- 16 |         StrictM j -> undefined
--    |         ^^^^^^^
--
-- test_24_a_2.hs:20:9: error: [GHC-76037]
--     Not in scope: data constructor ‘StrictC’
--    |
-- 20 |         StrictC j -> undefined
--    |         ^^^^^^^
--
-- Failed, one module loaded.


-- ghci> :l test_24_a_2.hs
-- [1 of 3] Compiling Module_24_a_2    ( Module_24_a_2.hs, interpreted )
-- [2 of 3] Compiling Main             ( test_24_a_2.hs, interpreted )
-- Ok, two modules loaded.
-- ghci> mStrict
-- 1
-- ghci> cStrict
-- 2


-- ghci> :l test_24_a_2.hs
-- [1 of 3] Compiling Module_24_a_2    ( Module_24_a_2.hs, interpreted )
-- [2 of 3] Compiling Main             ( test_24_a_2.hs, interpreted )
-- Ok, two modules loaded.
-- ghci> m1Strict
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   undefined, called at test_24_a_2.hs:59:14 in main:Main
-- ghci> m2Strict
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   undefined, called at test_24_a_2.hs:63:23 in main:Main
-- ghci> cStrict
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   undefined, called at test_24_a_2.hs:68:15 in main:Main
