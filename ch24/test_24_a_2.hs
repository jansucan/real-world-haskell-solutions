import Module_24_a_2
--import Control.Concurrent -- Non-strict versions


-- Test: The type constructors are accessible
-- mTypeConstructorAccessible :: IO (StrictMVar Int)
-- mTypeConstructorAccessible = newStrictEmptyMVar

-- cTypeConstructorAccessible :: IO (StrictChan Char)
-- cTypeConstructorAccessible = newStrictChan


-- Test: The data constructors are not accessible
-- mDataConstructorNotAccessible :: StrictMVar Int
-- mDataConstructorNotAccessible x = case x of
--                                     StrictM j -> undefined

-- cDataConstructorNotAccessible :: StrictChan Int
-- cDataConstructorNotAccessible x = case x of
--                                     StrictC j -> undefined


-- Test: It is not possible to change the import from the strict versions to
--       non-strict ones with this test still compiling
-- mStrict = do
--   x <- newStrictEmptyMVar
--   putMVar x 1
--   v <- takeMVar x
--   putStrLn $ show v

-- cStrict = do
--   x <- newStrictChan
--   writeChan x 2
--   v <- readChan x
--   putStrLn $ show v


-- Test: The non-strict versions do not evalute the value before placing it into
--       MVar or Chan
-- m1NonStrict = do
--    x <- newEmptyMVar
--    putMVar x undefined
--    return ()

-- m2NonStrict = do
--    x <- newMVar undefined
--    return ()

-- cNonStrict = do
--   x <- newChan
--   writeChan x undefined
--   return ()


-- Test: The strict versions evalute the value before placing it into MVar or
--       Chan
-- m1Strict = do
--    x <- newStrictEmptyMVar
--    putMVar x undefined
--    return ()

-- m2Strict = do
--    x <- newStrictMVar undefined
--    return ()

-- cStrict = do
--   x <- newStrictChan
--   writeChan x undefined
--   return ()
