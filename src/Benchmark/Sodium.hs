{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-binds #-}
module Benchmark.Sodium (
  benchmark1
, benchmark2
, benchmark3
, main
) where

import FRP.Sodium
import FRP.Sodium.Internal (ioReactive)
import Control.Monad
import Control.Applicative
import Data.Time
import System.Random.MWC
import qualified Data.IntMap as IM
import Text.Printf
import System.IO
import System.Mem
import Data.Maybe

benchmark1 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark1 netsize dur = do
  startTime <- getCurrentTime
  (stringEs, triggers) <- unzip <$> replicateM netsize (sync newEvent)
  let trigMap = IM.fromList $ zip [0..netsize-1] triggers
  -- 'merge' appears to be very slow for sodium
  -- let merged = foldl1' merge stringEs
  -- unreg <- sync $ listen merged (ePutStrLn)
  --
  -- This implementation is much faster
  unreg <- sync $ mapM (`listen` ePutStrLn) stringEs

  midTime <- getCurrentTime
  randGen <- create

  forM_ [1..dur] $ \step -> do
      let str = show step
      evs <- replicateM 10 $ uniformR (0,netsize-1) randGen
      sync $ forM_ evs (\ev -> maybe (error "sodium bench1: trigger not found") ($ str) $ IM.lookup ev trigMap)
  endTime <- getCurrentTime
  sequence_ unreg
  return (midTime `diffUTCTime` startTime, endTime `diffUTCTime` midTime)

benchmark2 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark2 netsize dur = do
  startTime <- getCurrentTime
  (unitEs, triggers) <- unzip <$> replicateM netsize (sync newEvent)
  (stepE, stepTrigger) <- sync newEvent
  let trigMap = IM.fromList $ zip [0..netsize-1] triggers
  let networkD = do
          countBs <- mapM (accum 0 . (const (1+) <$>)) unitEs
          let collector _ (b:bs@(_:_)) = (b,bs)
              collector _ ([b]) = (b,[b])
              collector _  _ = error "benchmark2, empty network"
          selectedB_E <- collectE collector (tail countBs) (filterE (\cnt -> cnt `rem` 10 == 0) stepE)
          switch =<< hold (head countBs) selectedB_E
  -- the currently selected behavior
  selectedB <- sync networkD

  midTime <- getCurrentTime
  randGen <- create

  forM_ [1..dur] $ \step -> do
      randomRs <- replicateM 10 $ uniformR (0,netsize-1) randGen

      val <- sync $ do
          stepTrigger step
          forM_ randomRs $ \ev -> maybe (error "sodium bench2: trigger not found") ($ ()) $ IM.lookup ev trigMap
          sample selectedB
      ePutStrLn (show (val :: Int))
  endTime <- getCurrentTime
  return (midTime `diffUTCTime` startTime, endTime `diffUTCTime` midTime)

data Box = Box (Behavior [Event Int])
fromBox :: Box -> Behavior (Event Int)
fromBox (Box eventB) = foldr merge never <$> eventB

benchmark3 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark3 netsize dur = do
  startTime <- getCurrentTime
  let err = error "sodium bench3: trigger not found"
  (intEs, triggers) <- unzip <$> replicateM netsize (sync newEvent)
  (stepE, stepTrigger) <- sync newEvent
  (randB, randTrigger) <- sync $ newBehavior 0
  let trigMap  = IM.fromList $ zip [0..netsize-1] triggers
      eventMap = IM.fromList $ zip [0..netsize-1] intEs
  let networkD = do
          let step10E = filterE (\cnt -> cnt `rem` 10 == 0) stepE

          let stateFunction randIx curEs = tail curEs ++ [fromMaybe err $ IM.lookup randIx eventMap]
          eventListStateE <- collectE (\t s -> let x' = stateFunction t s in (x',x')) (take 10 intEs) (snapshot (\_ b -> b) step10E randB)
          eventListStateB <- hold [] eventListStateE

          let box = Box eventListStateB
          return $ switchE $ fromBox box

  -- we need to return something from sync'ing the network, and keep a ref to
  -- it.  Otherwise the network will be garbage collected, and stuff will be
  -- dropped.  But we only need to run this network once, or else we'll make
  -- duplicate result Event streams.
  resultIntE <- sync networkD

  midTime <- getCurrentTime
  randGen <- create

  forM_ [1..dur] $ \step -> do
      rand <- uniformR (0,netsize-1) randGen
      unreg <- sync $ do
          stepTrigger step
          randTrigger rand
          replicateM_ (netsize `div` 2) $ do
              r <- ioReactive $ uniformR (0,netsize-1) randGen
              maybe (error "sodium bench2: trigger not found") ($ r) $ IM.lookup r trigMap
          listen resultIntE (ePutStrLn . show)
      unreg
  -- we want to print anything from the resultIntE stream, but we need to
  -- un-register the listener afterwards to prevent holding on to/printing from
  -- old streams.
  endTime <- getCurrentTime
  return (midTime `diffUTCTime` startTime, endTime `diffUTCTime` midTime)

main :: IO ()
main = do
    let testN restrictions (lbl,bench) netsize dur = do
            putStrLn $ printf "%s iterations: %d netsize: %d" lbl dur netsize
            if (dur,netsize) `elem` restrictions
                then putStrLn "...skipping"
                else do
                    performGC
                    (setup, run) <- bench netsize dur
                    putStrLn $ printf "setup: %s\nruntime: %s" (show setup) (show run)

    let benches = [("benchmark 1", benchmark1)
                  ,("benchmark 2", benchmark2)
                  ,("benchmark 3", benchmark3)]
        durs    = [100,1000]
        sizes   = [100,1000,10000,100000]
        restrictions = []
    sequence_ $ testN restrictions <$> benches <*> sizes <*> durs

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr
