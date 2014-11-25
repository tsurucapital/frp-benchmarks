{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-binds #-}
module Benchmark.Impulse (
  benchmark1
, benchmark2
, main
) where
import Reactive.Impulse
import Control.Applicative
import Control.Monad
import Data.Thyme
import Data.AffineSpace
import System.Random.MWC
import qualified Data.IntMap as IM
import Text.Printf
import System.IO
import System.Mem

benchmark1 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark1 netsize dur = do
  starttime <- getCurrentTime
  let networkD :: SGen (IM.IntMap (String -> IO ()))
      networkD = do
          (triggers, evs) <- unzip <$> replicateM netsize newAddHandler
          let trigMap = IM.fromList $ zip [0..netsize-1] triggers

          mapM_ (\ev -> reactimate $ ePutStrLn <$> ev) evs
          return trigMap
  (trigMap, _network) <- compileNetwork networkD
  midTime <- getCurrentTime

  randGen <- create
  forM_ [1..dur] $ \step -> do
      let str = show step
      replicateM_ 10 $ do
          ev <- uniformR (0,netsize-1) randGen
          maybe (error "not found!") ($ str) $ IM.lookup ev trigMap
  endTime <- getCurrentTime
  return (midTime .-. starttime, endTime .-. midTime)

benchmark2 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark2 netsize dur = do
  startTime <- getCurrentTime
  let networkD :: SGen (Int -> IO (), IM.IntMap (() -> IO ()))
      networkD = do
          (triggers, unitEs) <- unzip <$> replicateM netsize newAddHandler
          (stepTrigger, stepE) <- newAddHandler
          let trigMap = IM.fromList $ zip [0..netsize-1] triggers

          let countBs = map count unitEs

          let step10E = filterE' (\cnt -> cnt `rem` 10 == 0) stepE
          let selectedB_E = head <$> accumE countBs (keepTail <$ step10E)

          let selectedB = switchB (head countBs) selectedB_E
          let outputE = applyB stepE (const . ePutStrLn . ("Output " ++) . show <$> selectedB)
          reactimate outputE
          return (stepTrigger, trigMap)

  ((stepTrigger, trigMap), _network) <- compileNetwork networkD

  midTime <- getCurrentTime
  randGen <- create

  forM_ [1..dur] $ \step -> do
      randomRs <- replicateM 10 $ uniformR (0,netsize-1) randGen

      stepTrigger step
      forM_ randomRs $ \ev -> maybe (error "impulse bench2: trigger not found") ($ ()) $ IM.lookup ev trigMap
  endTime <- getCurrentTime
  return (midTime .-. startTime, endTime .-. midTime)

main :: IO ()
main = do
    let testN (lbl,bench) netsize dur = do
            putStrLn $ printf "%s iterations: %d netsize: %d" lbl dur netsize
            performGC
            (setup, run) <- bench netsize dur
            putStrLn $ printf "setup: %s\nruntime: %s" (show setup) (show run)
    let benches = [ ("benchmark 1", benchmark1)
                    -- ("benchmark 2", benchmark2)
                  ]
        durs    = [1000]
        sizes   = [10000]
        -- durs    = [100,1000]
        -- sizes   = [100,1000,10000]
    sequence_ $ testN <$> benches <*> sizes <*> durs


ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

count :: Event a -> Behavior Int
count = accumB 0 . ((\(!n) -> n+1) <$)

filterE' :: (a -> Bool) -> Event a -> Event a
filterE' p = filterE (\a -> a <$ guard (p a))

accumE :: a -> Event (a -> a) -> Event a
accumE a0 e = let prevB = stepper a0 thisE
                  thisE = applyE e prevB
              in thisE

keepTail :: [a] -> [a]
keepTail (_:y:zs) = y:zs
keepTail [x]      = [x]
keepTail []       = []
