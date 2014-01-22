{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Benchmark.Netwire
    ( benchmark1
    , benchmark2
    , test
    ) where
import Control.Monad (forM_, unless)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import Data.Monoid (Monoid(..))
import Prelude hiding ((.), id)
import System.IO

import Control.Wire hiding (loop, unless)
import System.Random.MWC (GenIO)
import qualified System.Random.MWC as MWC

inputsPerStep :: Int
inputsPerStep = 10

type Step e m a b
    = GenIO
    -> Int -- size of the network
    -> Int -- current step number
    -> Wire e m a b
    -> Session m
    -> m (Wire e m a b, Session m)

mkBenchmark
   :: Step e IO a b
   -> (Int -> Wire e IO a b)
   -> Int
   -> Int
   -> IO ()
mkBenchmark step wire netsize dur = do
    gen <- MWC.create
    loop gen 0 (wire netsize) clockSession
    where
        loop gen !stepNum wire session
            | stepNum >= dur = return ()
            | otherwise = do
                  (wire', session') <- step gen netsize stepNum wire session
                  loop gen (stepNum+1) wire' session'

------------------------------------------------------------
-- Test 1

benchmark1 :: Int -> Int -> IO ()
benchmark1 = mkBenchmark step1 wire1

wire1
    :: Monad m
    => Int
    -> Wire e m (Int, String) String
wire1 netsize = unicast wires
    where
        wires = IntMap.fromList $ zip [0..] $ replicate netsize id

step1 :: Step e IO (Int, String) String
step1 gen netsize n wire (Session update) = do
    (dt, session') <- update
    wire' <- loop dt inputsPerStep wire
    return (wire', session')
    where
        loop dt i w
            | i <= 0 = return w
            | otherwise = do
                  index <- MWC.uniformR (0, netsize-1) gen
                  (r, wire') <- stepWire w dt (index, show n)
                  case r of
                      Left _ -> return ()
                      Right x -> hPutStrLn stderr x
                  loop dt (i-1) wire'

------------------------------------------------------------
-- Test 2

benchmark2 :: Int -> Int -> IO ()
benchmark2 = mkBenchmark step2 wire2

wire2
    :: Monad m
    => Int
    -> Wire e m (Int, ()) Int
wire2 netsize = unicastWithSwitch 10 wires (pure 0)
    where
        wires = IntMap.fromList $ zip [0..] (replicate netsize counter)

counter :: Monad m => Wire e m () Int
counter = accum (\n _ -> n + 1) 0

step2 :: Step e IO (Int, ()) Int
step2 gen netsize _n wire (Session update) = do
    (dt, session') <- update
    (r, wire') <- loop dt inputsPerStep (Right 0) wire
    case r of
        Left _ -> return ()
        Right x -> hPrint stderr x
    return (wire', session')
    where
        loop dt i r w
            | i <= 0 = return (r, w)
            | otherwise = do
                  index <- MWC.uniformR (0, netsize-1) gen
                  (r', wire') <- stepWire w dt (index, ())
                  loop dt (i-1) r' wire'

------------------------------------------------------------
-- Wires and combinators

-- | Construct a combined wire from a vector of wires.
-- The constructed wire takes an indexed value as input,
-- and emit corresponding output.
unicast
    :: Monad m
    => IntMap (Wire e m a b)
    -> Wire e m (Int, a) b
unicast wires = mkGen $ \dt (idx, a) -> do
    (r, wire') <- stepWire (wires ! idx) dt a
    return (r, unicast (IntMap.insert idx wire' wires))

unicastWithSwitch
    :: forall e m a b. (Monad m)
    => Int
    -- ^ Switching interval
    -> IntMap (Wire e m a b)
    -> Wire e m (Int, a) b
    -- ^ Initial wire
    -> Wire e m (Int, a) b
unicastWithSwitch interval wires0 = switch (genWire 0 wires0)
    where
        genWire
            :: Int
            -- ^ Current step count
            -> IntMap (Wire e m a b)
            -- ^ Current set of wires
            -> Wire e m (Int, a) (Wire e m (Int, a) b)
        genWire !n wires = mkGen $ \dt (idx, a) -> do
            (r, wire') <- stepWire (wires ! idx) dt a
            let wires' = IntMap.insert idx wire' wires
                idx' = n `div` (interval * inputsPerStep)
            -- liftIO $ putStrLn $ unwords
            --     [ show idx' ++ " - " ++ show n ++ "/" ++ show interval ++ ":"
            --     , "change: " ++ show idx ++ " -> " ++ show r
            --     ]
            return ( Right (lmap snd (wires ! idx'))
                   , genWire (n+1) wires'
                   )

------------------------------------------------------------
-- Helper functions

test n wire = loop 0 wire clockSession
    where
        loop i w s = do
            (r, w', s') <- stepSession w s ()
            print r
            unless (i > n) $ loop (i+1) w' s'
