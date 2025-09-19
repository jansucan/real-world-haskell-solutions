-- The Chan type is implemented using MVars. Use MVars to develop a BoundedChan
-- library.
--
-- Your newBoundedChan function should accept an Int parameter, limiting the
-- number of unread items that can be present in a BoundedChan at once.
--
-- If this limit is hit, a call to your writeBoundedChan function must block
-- until a reader uses readBoundedChan to consume a value.


-- For simplicity
--   - I don't consider asynchronous exceptions in the design
--   - I assume that the bounded channel capacity is > 0

-- For this exercise, I set more strict requirements than necessary:
--   - The size of the BoundedChan data structure must be constant except for
--     the underlying unbounded channel. It means that all the members of the
--     BoundedChannel record, except for the unbounded channel, must not grow in
--     size with growing number of writers and readers using the BoundedChannel.
--
--   - The ordering of writers and readers must be kept. It means, for example,
--     that when a channel is full and the writer starts sleeping and waiting
--     for being notified that it can write to the channel, no other writer has
--     written to the channel while the woken up writer was sleeping.


-- From Haskell documentation for MVar, when multiple threads are blocked on it,
-- they are woken up in FIFO order. This means that we can think of MVar also as
-- a queue.
--
-- For a better idea about the algorithm, here is a picture:
--
--      +- Room ----------------+
--      |                       |
--      |       +-------+       |
--      |       | State |       |
--      |       +-------+       |
--      |                       |
--      |   +---+       +---+   |
--      |   | W |       | R |   |
--      |   +---+       +---+   |
--      |                       |
--      +--^-----------------^--+
--         |                 |
--    +---------+       +---------+
--    | Writers |       | Readers |
--    +----+----+       +---------+
--         |                 |
--         |                 |
--
--
-- The room is a space where at most two threads can be present: one writer and
-- one reader.
--
-- There are five MVars:
--   - Writers is where writer threads queue up for entering the room
--   - Readers is where reader threads queue up for entering the room
--   - W is a chair where a writer in the room waits/sleeps when the channel is
--     full
--   - R is a chair where a reader in the room waits/sleeps when the channel is
--     empty
--   - State synchronizes access the the bounded channel state information
--     between the reader and the writer in the room
--
-- The rest of the algorithm is described in a comments in the source code
-- below. I comment only the writeBoundedChan. The readBoundedChan is analogous.

import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Control.Concurrent (forkIO, threadDelay) -- For testing

data WriterState = WriterWaiting | NoWriter
  deriving(Eq)

data ReaderState = ReaderWaiting | NoReader
  deriving(Eq)

data BoundedChannel a = BoundedChannel {
  mvState :: MVar (Int, Int, WriterState, ReaderState),
  mvW :: MVar (),
  mvR :: MVar (),
  mvWriters :: MVar (),
  mvReaders :: MVar (),
  chan :: Chan a
  }

newBoundedChan :: Int -> IO (BoundedChannel a)
newBoundedChan max = do
  mst <- newMVar (0, max, NoWriter, NoReader)
  mw <- newEmptyMVar
  mr <- newEmptyMVar
  mws <- newMVar ()
  mrs <- newMVar ()
  ch <- newChan

  return $ BoundedChannel mst mw mr mws mrs ch


writeBoundedChan :: BoundedChannel a -> a -> IO ()
writeBoundedChan bc val =
  -- Only one writer can enter the room. When it exits the room, it allows
  -- another writer to come in.
  withMVar (mvWriters bc) (\_ -> inTheRoom bc val)
  where
    getState = do
      -- We can ignore WriterState because we are the only writer in the room
      -- now and we know we are not waiting
      (n, max, _, readerWaiting) <- takeMVar (mvState bc)
      -- Every time we get the state, we are going to write a value. We must
      -- wake up any reader waiting for a value in the channel.
      case readerWaiting of
        ReaderWaiting -> putMVar (mvR bc) ()
        _ -> pure ()
      return (n, max)

    putState n max wrWaiting= do
      -- We already woken up a reader and still holding the state, so no reader
      -- could get it, check it, and start waiting. Set its state to NoReader.
      putMVar (mvState bc) (n, max, wrWaiting, NoReader)

    inTheRoom bc val = do
      (n, max) <- getState
      if n < max
        then do
          -- There is free space in the channel. Write the value.
          writeChan (chan bc) val
          -- Update the number of values in the channel and release the state to
          -- allow another reader or writer in the room to take it
          putState (n+1) max NoWriter
        else do
          -- No free space. Release the state not to block a reader and let it
          -- know we are waiting.
          putState n max WriterWaiting
          -- We must wait until a reader frees the space and wakes us up
          takeMVar (mvW bc)

          -- We were woken up by a reader. The channel is guaranteed to contain
          -- space for our value. Because while we are in the room, no other
          -- writers are allowed to enter. Only readers could enter and they
          -- just take values from the channel.

          -- Get the current state because we don't know what happened while we
          -- were sleeping
          (n, max) <- getState

          writeChan (chan bc) val
          putState (n+1) max NoWriter


readBoundedChan :: BoundedChannel a -> IO a
readBoundedChan bc = do
  withMVar (mvReaders bc) (\_ -> inTheRoom bc)
  where
    getState = do
      (n, max, writerWaiting, _) <- takeMVar (mvState bc)
      case writerWaiting of
        WriterWaiting -> putMVar (mvW bc) ()
        _ -> pure ()
      return (n, max)

    putState n max rdWaiting = do
      putMVar (mvState bc) (n, max, NoWriter, rdWaiting)

    inTheRoom bc = do
      (n, max) <- getState
      if n > 0
        then do
          val <- readChan (chan bc)
          putState (n-1) max NoReader
          return val
        else do
          putState n max ReaderWaiting
          takeMVar (mvR bc)

          (n, max) <- getState

          val <- readChan (chan bc)
          putState (n-1) max NoReader
          return val




-- ghci> :l 24_a_1.hs
-- [1 of 2] Compiling Main             ( 24_a_1.hs, interpreted )
-- Ok, one module loaded.

testWriterDoesNotBlockWhenChan3NotFull = do
  ch <- newBoundedChan 3 :: IO (BoundedChannel Int)
  writeBoundedChan ch 1
  writeBoundedChan ch 2
  writeBoundedChan ch 3
  return ()

-- ghci> testWriterDoesNotBlockWhenChan3NotFull
-- ghci>


testWriterBlocksWhenChan3Full = do
  ch <- newBoundedChan 3 :: IO (BoundedChannel Int)
  writeBoundedChan ch 1
  writeBoundedChan ch 2
  writeBoundedChan ch 3
  writeBoundedChan ch 4
  return ()

-- ghci> testWriterBlocksWhenChan3Full
-- *** Exception: thread blocked indefinitely in an MVar operation


testReaderDoesNotBlockWhenChan5NotEmpty = do
  ch <- newBoundedChan 5 :: IO (BoundedChannel Int)
  writeBoundedChan ch 1
  readBoundedChan ch
  return ()

-- ghci> testReaderDoesNotBlockWhenChan5NotEmpty
-- ghci>


testReaderBlocksWhenChan5Empty = do
  ch <- newBoundedChan 5 :: IO (BoundedChannel Int)
  readBoundedChan ch
  return ()

-- ghci> testReaderBlocksWhenChan5Empty
-- *** Exception: thread blocked indefinitely in an MVar operation


testMultipleThreadWritersReaders = do
  ch <- newBoundedChan 2 :: IO (BoundedChannel String)

  mvRd1 <- newEmptyMVar :: IO (MVar [String])
  mvRd2 <- newEmptyMVar :: IO (MVar [String])
  mvRd3 <- newEmptyMVar :: IO (MVar [String])
  mvRd4 <- newEmptyMVar :: IO (MVar [String])

  forkIO $ do
    writeBoundedChan ch "1"
    writeBoundedChan ch "2"
    writeBoundedChan ch "3"
  forkIO $ do
    writeBoundedChan ch "a"
    writeBoundedChan ch "b"
    writeBoundedChan ch "c"
    writeBoundedChan ch "d"
    writeBoundedChan ch "e"
  forkIO $ do
    writeBoundedChan ch "W"
    writeBoundedChan ch "X"
    writeBoundedChan ch "Y"
    writeBoundedChan ch "Z"

  forkIO $ do
    a <- readBoundedChan ch
    b <- readBoundedChan ch
    c <- readBoundedChan ch
    d <- readBoundedChan ch
    putMVar mvRd1 [a, b, c, d]
  forkIO $ do
    a <- readBoundedChan ch
    b <- readBoundedChan ch
    putMVar mvRd2 [a, b]
  forkIO $ do
    a <- readBoundedChan ch
    putMVar mvRd3 [a]
  forkIO $ do
    a <- readBoundedChan ch
    b <- readBoundedChan ch
    c <- readBoundedChan ch
    d <- readBoundedChan ch
    e <- readBoundedChan ch
    putMVar mvRd4 [a, b, c, d, e]

  -- Wait for the reader threads to exit and collect the results
  l1 <- takeMVar mvRd1
  l2 <- takeMVar mvRd2
  l3 <- takeMVar mvRd3
  l4 <- takeMVar mvRd4
  let l = l1 ++ l2 ++ l3 ++ l4
  putStrLn $ concat l

-- ghci> testMultipleThreadWritersReaders
-- 123XacWbYdZe
-- ghci> testMultipleThreadWritersReaders
-- 13cd2baWXYZe
-- ghci> testMultipleThreadWritersReaders
-- abcXW21deY3Z
-- ghci> testMultipleThreadWritersReaders
-- 1bcdaXW23YZe
