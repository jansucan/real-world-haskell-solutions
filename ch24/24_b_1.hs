-- 1. It can be difficult to determine when to switch from parSort2 to sort. An
--    alternative approach to the one we outline above would be to decide based
--    on the length of a sublist. Rewrite parList2 so that it switches to sort
--    if the list contains more than some number of elements.
--
-- 2. Measure the performance of the length-based approach, and compare with the
--    depth approach. Which gives better performance results?


-- There is most probably a typo in the assignment. The
--   "if the list contains more than"
-- should be
--   "if the list contains less than".
--
-- parSort2 switches to sort if the level is deeper than a specified level. The
-- deeper the level is, the shorter the sublist is. It makes sense to use the
-- sequential sort for a short list because the overhead of creating threads
-- would be too high in proportion to just sequentially sorting the list.
--
-- partList2 should do that too (switching to sort if the list is shorter than
-- the threshold), it should just decide based on the length of a sublist.


-- Comparing performance of algorithms is not easy, especially for average cases
-- of inputs. Thus, instead of in-depth analysis and comparison I will just do a
-- few measurements and based on that I will set a general result. For the
-- comparison to make more sense, both of the implementations will run on the
-- same random input.


{-- From examples/examples/ch24/SortMain.hs and modified --}
module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)

import Sorting

import Control.Parallel (par, pseq)

randomInts :: Int -> StdGen -> [Int]
randomInts k g = let result = take k (randoms g)
                 in force result `seq` result

main = do
  args <- getArgs
  let (inputSize, paramSort, paramList, avgRuns) =
        case args of
          (c:ps:pl:a:_) -> (read c, read ps, read pl, read a)
          _ -> error "Missing arguments"
  input <- randomInts inputSize `fmap` getStdGen
  putStrLn $ "We have " ++ show (length input) ++ " elements to sort."

  s <- timedAvg (parSort2 paramSort) input avgRuns
  putStrLn $ show s

  t <- timedAvg (parList2 paramList inputSize) input avgRuns
  putStrLn $ show t

timedAvg :: ([Int] -> [Int]) -> [Int] -> Int -> IO NominalDiffTime
timedAvg sortFunc input runCount = do
  times <- mapM (\_ -> timed sortFunc input) [1..runCount]
  return $ (sum times) / (fromIntegral (length times))

timed :: ([Int] -> [Int]) -> [Int] -> IO NominalDiffTime
timed sortFunc input = do
  start <- getCurrentTime
  let sorted = sortFunc input
  putStr $ "Sorted all " ++ show (length sorted) ++ " in "
  end <- getCurrentTime
  let diff = end `diffUTCTime` start
  putStrLn $ show diff
  return $ diff
{-- End of code from examples --}

parList2 :: (Ord a) => Int -> Int -> [a] -> [a]
parList2 l min list@(x:xs)
  | l < min   = sort list
  | otherwise = force greater `par` (force lesser `pseq`
                                      (lesser ++ x:greater))
  where
        le      = [y | y <- xs, y <  x]
        gr      = [y | y <- xs, y >= x]
        leLen   = length le
        grLen   = length gr
        lesser  = parList2 leLen min le
        greater = parList2 grLen min gr
parList2 _ _ _ = []


-- Choice of pivot for Quicksort is very important. In the parSort2
-- implementation from the examples, the pivot is the first item in a list to
-- sort.
--
-- This means, a sorted list is the worst case. For ascending order, the
-- 'lesser' array will always contain no elements, the 'greater' list will
-- always contain all the elements. Thus, the sorting will be slowest (time
-- complexity O(n^2)). Similarly for descending order.
--
-- The best case is when the first item in the list is the middle value, it
-- means, when half of the items in the list are smaller and half of them bigger
-- than the middle value. This results in a balanced split of the sorted list
-- and the sorting depth will be log(n) (time complexity O(n log(n))).


-- My hypothesis was that for the best case, parSort2 and parList2 would perform
-- similarly because a depth limit for parSort2 can be converted into a list
-- length limit for parList2, so both would be switching to the sequential
-- 'sort' at the same time.
--
-- The closest the input list would be to the worst case, the more difference
-- would there be. parList2 would perform much better thanks to fewer
-- applications of 'par'.
--
-- After running a few worst case experiments I found out that the quadratic
-- time complexity by far outweighs the speedup gained from reducing
-- applications of 'par', so I used only average/random inputs.


-- I compiled the source with
--   stack ghc -- -threaded 24_b_1.hs

-- My computer has 4 cores. The command I used for running the sort was
--   ./24_b_1 +RTS -N4 -RTS <INPUT_SIZE> <PARSORT_PARAM> <PARLIST_PARAM> <AVG_RUNS>
--
-- where INPUT_SIZE is number of elements of the list to sort, PARSORT_PARAM is
-- the parameter to parSort2 (the threshold depth), PARLIST_PARAM is the
-- parameter to parList2 (the threshold length), and AVG_RUNS is number of runs
-- of each sort implementation for computing the average sorting time.

-- The input sizes tested were 4 000 000 and 400 000 elements. The testing was
-- automated using a shell script. I set the parameters by computing binary
-- logarithm from the size of the input. This is the max. depth of a balanced
-- binary tree, it means, max. depth for a best case list. In the result tables
-- below this computed parameter is in parentheses. Then I tested lower and a
-- few greater parameter values. For parSort2 the value is the depth, for
-- parList2 it is a length of a sublist + 1 (because of 'l < min' in the
-- implementation I wanted the switch to the sequential sort to happen also at
-- the threshold value).

-- The results for input size 4 000 000:
--
-- PARSORT_PARAM  PARLIST_PARAM   parSort2_time   parList2_time  is_parList2_faster
--            26       67108865  16.1435506608s  16.5742411336s
--            24       16777217  17.2344045572s  15.0797320632s  yes
--          (22)      (4194305)  16.747620797s   17.9526976908s
--            20        1048577  15.8036009976s  14.9289581244s  yes
--            18         262145  14.9604795514s  15.0083064052s
--            16          65537  13.5984633668s  14.8022182528s
--            14          16385  15.8516987842s  17.8260459492s
--            12           4097  15.4590705512s  15.6482811018s
--            10           1025  13.8612868966s  14.5261572668s
--             8            257  13.1958234944s  14.6600659704s
--             6             65  15.4553144422s  14.8244605894s  yes
--             4             17  13.5678723824s  15.7817085602s
--             2              5  14.6244923772s  15.7249399864s
--             0              2  15.7628948616s  15.7540970314s  yes

-- The results for input size 400 000:
--
-- PARSORT_PARAM  PARLIST_PARAM  parSort2_time  parList2_time  is_parList2_faster
--            26       67108865  0.808638032s   0.8980840812s
--            24       16777217  0.9464536416s  1.0021756256s
--          (22)      (4194305)  0.9558710392s  1.0741807468s
--            20        1048577  0.764964904s   0.9638306786s
--            18         262145  1.0843675078s  1.1163900494s
--            16          65537  0.92859031s    0.96355397s
--            14          16385  0.9129570608s  1.0464927062s
--            12           4097  1.1209530828s  1.1124485972s  yes
--            10           1025  0.8144274202s  0.987780161s
--             8            257  1.0196146734s  1.0743890808s
--             6             65  0.9790765378s  0.9524003582s  yes
--             4             17  0.8646994942s  0.902187454s
--             2              5  0.8896786326s  0.9591653616s
--             0              2  1.0028687866s  0.9903121374s  yes

-- Overall, parSort2 performs better than parList2.
