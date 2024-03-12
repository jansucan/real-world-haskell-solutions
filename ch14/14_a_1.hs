-- Rewrite getRandom to use do notation.

import Control.Monad.State
import System.Random

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val

getRandomDo :: Random a => RandomState a
getRandomDo = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val


-- ghci> :l 14_a_1.hs
-- [1 of 1] Compiling Main             ( 14_a_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> runState getRandomDo (mkStdGen 1)
-- (-2241774542048937483,StdGen {unStdGen = SMGen 4999253871718377453 10451216379200822465})

-- ghci> (runState getRandom (mkStdGen 1)) == (runState getRandomDo (mkStdGen 234))
-- False
-- ghci> (runState getRandom (mkStdGen 1)) == (runState getRandomDo (mkStdGen 1))
-- True
