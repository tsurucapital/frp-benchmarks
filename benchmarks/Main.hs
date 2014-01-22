module Main where

import Criterion
import Criterion.Main (defaultMain)

import qualified Benchmark.Netwire as Netwire
import qualified Benchmark.Banana as Banana
import qualified Benchmark.Sodium as Sodium
import qualified Benchmark.Euphoria as Euphoria
import qualified Benchmark.Ordrea as Ordrea

main :: IO ()
main = defaultMain
    -- Benchmark 1
    [ bgroup "benchmark1/100-nodes-100-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark1 100 100
          -- , bench "banana" $ whnfIO $ Banana.benchmark1 100 100
          , bench "sodium" $ whnfIO $ Sodium.benchmark1 100 100
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark1 100 100
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark1 100 100
          ]
    , bgroup "benchmark1/100-nodes-1000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark1 100 1000
          -- , bench "banana" $ whnfIO $ Banana.benchmark1 100 100
          , bench "sodium" $ whnfIO $ Sodium.benchmark1 100 1000
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark1 100 1000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark1 100 1000
          ]
    , bgroup "benchmark1/1000-nodes-100-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark1 1000 100
          -- , bench "banana" $ whnfIO $ Banana.benchmark1 1000 100
          , bench "sodium" $ whnfIO $ Sodium.benchmark1 1000 100
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark1 1000 100
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark1 1000 100
          ]
    , bgroup "benchmark1/1000-nodes-1000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark1 1000 1000
          -- , bench "banana" $ whnfIO $ Banana.benchmark1 1000 1000
          , bench "sodium" $ whnfIO $ Sodium.benchmark1 1000 1000
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark1 1000 1000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark1 1000 1000
          ]
    , bgroup "benchmark1/1000-nodes-10000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark1 1000 10000
          -- , bench "banana" $ whnfIO $ Banana.benchmark1 1000 10000
          , bench "sodium" $ whnfIO $ Sodium.benchmark1 1000 10000
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark1 1000 10000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark1 1000 10000
          ]
    , bgroup "benchmark1/10000-nodes-100-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark1 10000 100
          -- , bench "banana" $ whnfIO $ Banana.benchmark1 10000 100
          , bench "sodium" $ whnfIO $ Sodium.benchmark1 10000 100
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark1 10000 100
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark1 10000 100
          ]
    , bgroup "benchmark1/10000-nodes-1000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark1 10000 1000
          -- , bench "banana" $ whnfIO $ Banana.benchmark1 10000 1000
          , bench "sodium" $ whnfIO $ Sodium.benchmark1 10000 1000
          -- , bench "euphoria" $ whnfIO $ Euphoria.benchmark1 10000 1000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark1 10000 1000
          ]

    -- Benchmark 2
    , bgroup "benchmark2/100-nodes-100-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 100 100
          -- , bench "banana" $ whnfIO $ Banana.benchmark2 100 100
          , bench "sodium" $ whnfIO $ Sodium.benchmark2 100 100
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark2 100 100
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 100 100
          ]
    , bgroup "benchmark2/100-nodes-1000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 100 1000
          , bench "sodium" $ whnfIO $ Sodium.benchmark2 100 1000
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark2 100 1000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 100 1000
          ]
    , bgroup "benchmark2/1000-nodes-100-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 1000 100
          , bench "sodium" $ whnfIO $ Sodium.benchmark2 1000 100
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark2 1000 100
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 1000 100
          ]
    , bgroup "benchmark2/1000-nodes-1000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 1000 1000
          , bench "sodium" $ whnfIO $ Sodium.benchmark2 1000 1000
          , bench "euphoria" $ whnfIO $ Euphoria.benchmark2 1000 1000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 1000 1000
          ]
    , bgroup "benchmark2/1000-nodes-10000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 1000 10000
          , bench "sodium" $ whnfIO $ Sodium.benchmark2 1000 10000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 1000 10000
          ]
    , bgroup "benchmark2/10000-nodes-100-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 10000 100
          , bench "sodium" $ whnfIO $ Sodium.benchmark2 10000 100
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 10000 100
          ]
    , bgroup "benchmark2/10000-nodes-1000-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 10000 1000
          , bench "sodium" $ whnfIO $ Sodium.benchmark2 10000 1000
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 10000 1000
          ]
    , bgroup "benchmark2/100000-nodes-100-steps"
          [ bench "netwire" $ whnfIO $ Netwire.benchmark2 100000 100
          , bench "ordrea" $ whnfIO $ Ordrea.benchmark2 100000 100
          ]
    ]
