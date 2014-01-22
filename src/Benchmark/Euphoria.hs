
{-# OPTIONS -Wall -fno-warn-unused-binds #-}
module Benchmark.Euphoria (
  benchmark1
, benchmark2
, main
) where

import FRP.Euphoria.Event
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Time
import System.Random.MWC
import qualified Data.IntMap as IM
import Text.Printf
import System.IO
import System.Mem

benchmark1 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark1 netsize dur = do
    starttime <- getCurrentTime
    (eventGens, sinks) <- unzip <$> replicateM netsize externalEvent
    let sinkMap = IM.fromList $ zip [0..netsize - 1] sinks
    let network :: SignalGen (Signal [String])
        network = eventToSignal . mconcat <$> sequence eventGens
    sample <- start network
    void sample
    midtime <- getCurrentTime
    randGen <- create
    forM_ [1..dur] $ \n -> do
        let str = show n
        replicateM_ 10 $ do
            ev <- uniformR (0,netsize-1) randGen
            maybe (return ()) ($ str) $ IM.lookup ev sinkMap
        mapM_ ePutStrLn =<< sample
    endtime <- getCurrentTime
    return (midtime `diffUTCTime` starttime, endtime `diffUTCTime` midtime)

benchmark2 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark2 netsize dur = do
  startTime <- getCurrentTime
  (unitEventGens, sinks) <- unzip <$> replicateM netsize externalEvent
  (stepEventGen, stepSink) <- externalEvent
  let sinkMap = IM.fromList $ zip [0..netsize-1] sinks
  let network :: SignalGen (Signal Int)
      network = do
          countDs <- mapM (>>= count) unitEventGens
          stepE   <- stepEventGen
          let collector (d:ds@(_:_)) = (ds,d)
              collector ([d]) = ([d],d)
              collector  _ = error "benchmark2, empty network"
          selectedD_E <- scanAccumE (tail countDs) . (collector <$) $ filterE (\cnt -> cnt `rem` 10 == 0) stepE
          discreteToSignal . join =<< stepperD (head countDs) selectedD_E

  sample <- start network
  midTime <- getCurrentTime
  randGen <- create
  forM_ [1..dur] $ \step -> do
      stepSink step
      replicateM_ 10 $ do
          ev <- uniformR (0,netsize-1) randGen 
          maybe (error "euphoria bench2: sink not found") ($ ()) $ IM.lookup ev sinkMap

      (ePutStrLn . show) =<< sample
  endTime <- getCurrentTime
  return (midTime `diffUTCTime` startTime, endTime `diffUTCTime` midTime)



main :: IO ()
main = do
    let testN (lbl,bench) netsize dur = do
            putStrLn $ printf "%s iterations: %d netsize: %d" lbl dur netsize
            performGC
            (setup, run) <- bench netsize dur
            putStrLn $ printf "setup: %s\nruntime: %s" (show setup) (show run)

    let benches = [("benchmark 1", benchmark1)
                  ,("benchmark 2", benchmark2)
                  ]
        durs    = [100, 1000]
        sizes   = [100,1000,10000,100000]
    sequence_ $ testN <$> benches <*> sizes <*> durs

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

count :: Event a -> SignalGen (Discrete Int)
count =  accumD 0 . ((+1) <$)
