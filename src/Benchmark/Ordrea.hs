{-# OPTIONS -Wall -fno-warn-unused-binds #-}
module Benchmark.Ordrea (
  benchmark1
, benchmark2
, main
) where

import FRP.Ordrea
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Time
import System.Random.MWC
import qualified Data.IntMap as IM
import Text.Printf
import System.IO

benchmark1 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark1 netsize dur = do
    starttime <- getCurrentTime
    ees <- replicateM netsize newExternalEvent
    let sinkMap = IM.fromList $ zip [0..netsize - 1] ees
    let network :: SignalGen (Signal [String])
        network = eventToSignal . mconcat <$> mapM externalE ees
    sample <- start network
    void sample
    midtime <- getCurrentTime
    randGen <- create
    forM_ [1..dur] $ \n -> do
        let str = show n
        replicateM_ 10 $ do
            ev <- uniformR (0,netsize-1) randGen
            maybe (return ()) (flip triggerExternalEvent str) $ IM.lookup ev sinkMap
        mapM_ ePutStrLn =<< sample
    endtime <- getCurrentTime
    return (midtime `diffUTCTime` starttime, endtime `diffUTCTime` midtime)

benchmark2 :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark2 netsize dur = do
    starttime <- getCurrentTime
    ees <- replicateM netsize newExternalEvent
    let sinkMap = IM.fromList $ zip [0..netsize - 1] ees
    sample <- start $ do
        es <- mapM externalE ees
        bs <- mapM (accumD (0::Int) . (succ <$)) es
        stepNumE <- scanE (0::Int) $ succ <$ stepClockE
        selected <- fmap head <$> accumD bs (tail <$ filterE ((==0) . (`rem`10)) stepNumE)
        discreteToSignal <$> joinDD selected
    void sample
    midtime <- getCurrentTime
    randGen <- create
    replicateM_ dur $ do
        replicateM_ 10 $ do
            ev <- uniformR (0,netsize-1) randGen
            maybe (return ()) (flip triggerExternalEvent ()) $ IM.lookup ev sinkMap
        ePutStrLn . show =<< sample
    endtime <- getCurrentTime
    return (midtime `diffUTCTime` starttime, endtime `diffUTCTime` midtime)

main :: IO ()
main = do
    let testN (lbl,bench) netsize dur = do
            putStrLn $ printf "%s iterations: %d netsize: %d" lbl dur netsize
            (setup, run) <- bench netsize dur
            putStrLn $ printf "setup: %s\nruntime: %s" (show setup) (show run)

    let benches =
            [ ("benchmark 1", benchmark1)
            , ("benchmark 2", benchmark2)
            ]
        durs    = [100, 1000]
        sizes   = [100,1000,10000,100000]
    sequence_ $ testN <$> benches <*> sizes <*> durs

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr
