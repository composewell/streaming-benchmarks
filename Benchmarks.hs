-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Benchmarks.BenchmarkTH (createBgroup, createBgroupN)
import Benchmarks.Common (benchIO)

import qualified Benchmarks.Vector as Vector
import qualified Benchmarks.Streamly as Streamly
import qualified Benchmarks.Streaming as Streaming
import qualified Benchmarks.Machines as Machines
import qualified Benchmarks.Pipes as Pipes
import qualified Benchmarks.Conduit as Conduit
import qualified Benchmarks.Drinkery as Drinkery
import qualified Benchmarks.List as List
import qualified Benchmarks.VectorPure as VectorPure
-- import qualified Benchmarks.LogicT as LogicT
-- import qualified Benchmarks.ListT as ListT
-- import qualified Benchmarks.ListTransformer as ListTransformer

import Gauge

main :: IO ()
main = do
  defaultMain
    [ bgroup "elimination"
      [ $(createBgroup "drain" "toNull")
      , $(createBgroup "toList" "toList")
      , $(createBgroup "fold" "foldl")
      , $(createBgroup "last" "last")
      ]
    , bgroup "transformation"
      [ $(createBgroupN "scan" "scan" 1)
      , $(createBgroupN "map" "map" 1)
      , $(createBgroupN "mapM" "mapM" 1)
      ]
    , bgroup "transformationN"
      [ $(createBgroupN "scan" "scan" 4)
      , $(createBgroupN "map" "map" 4)
      , $(createBgroupN "mapM" "mapM" 4)
      ]
    , bgroup "filtering"
      [ $(createBgroupN "filter-all-out" "filterAllOut" 1)
      , $(createBgroupN "filter-all-in" "filterAllIn" 1)
      , $(createBgroupN "drop-all" "dropAll" 1)
      , $(createBgroupN "takeWhile-true" "takeWhileTrue" 1)
      , $(createBgroupN "filter-even" "filterEven" 1)
      , $(createBgroupN "take-all" "takeAll" 1)
      , $(createBgroupN "drop-one" "dropOne" 1)
      , $(createBgroupN "dropWhile-true" "dropWhileTrue" 1)
      , $(createBgroupN "dropWhile-false" "dropWhileFalse" 1)
      ]
    , bgroup "filteringN"
      [ $(createBgroupN "filter-even" "filterEven" 4)
      , $(createBgroupN "filter-all-out" "filterAllOut" 4)
      , $(createBgroupN "filter-all-in" "filterAllIn" 4)
      , $(createBgroupN "take-all" "takeAll" 4)
      , $(createBgroupN "takeWhile-true" "takeWhileTrue" 4)
      , $(createBgroupN "drop-one" "dropOne" 4)
      , $(createBgroupN "drop-all" "dropAll" 4)
      , $(createBgroupN "dropWhile-true" "dropWhileTrue" 4)
      , $(createBgroupN "dropWhile-false" "dropWhileFalse" 4)
      ]
    , bgroup "composed"
      [ $(createBgroupN "scan-map" "scanMap" 4)
      , $(createBgroupN "drop-map" "dropMap" 4)
      , $(createBgroupN "drop-scan" "dropScan" 4)
      , $(createBgroupN "take-drop" "takeDrop" 4)
      , $(createBgroupN "take-scan" "takeScan" 4)
      , $(createBgroupN "take-map" "takeMap" 4)
      , $(createBgroupN "filter-drop" "filterDrop" 4)
      , $(createBgroupN "filter-take" "filterTake" 4)
      , $(createBgroupN "filter-scan" "filterScan" 4)
      , $(createBgroupN "filter-map" "filterMap" 4)
      ]
    , $(createBgroup "zip" "zip")
    , $(createBgroup "concat" "concat")
    , bgroup "append"
      [ benchIO "streamly" Streamly.appendSource Streamly.toNull
      , benchIO "conduit" Conduit.appendSource Conduit.toNull
      -- append benchmark for all these packages hangs because of
      -- quadratic performance slowdown.
--    , benchIO "pipes" Pipes.appendSource Pipes.toNull
--    , bench "pipes" $ nfIO (return 1 :: IO Int)
--    , benchIO "vector" Vector.appendSource Vector.toNull
--    , bench "vector" $ nfIO (return 1 :: IO Int)
--    , benchIO "streaming" Streaming.appendSource Streaming.toNull
--    , bench "streaming" $ nfIO (return 1 :: IO Int)
      ]
      -- Perform 100,000 mapM recursively over a stream of length 10
      -- implemented only for vector and streamly.
    , bgroup "iterated/mapM"
      [ benchIO "streamly" Streamly.iterateMapM Streamly.toNull
      , benchIO "vector" Vector.iterateMapM Vector.toNull
      ]
    , bgroup "iterated/scan"
      [ benchIO "streamly" Streamly.iterateScan Streamly.toNull
      , benchIO "vector" Vector.iterateScan Vector.toNull
      ]
    , bgroup "iterated/filterEven"
      [ benchIO "streamly" Streamly.iterateFilterEven Streamly.toNull
      , benchIO "vector" Vector.iterateFilterEven Vector.toNull
      ]
    , bgroup "iterated/takeAll"
      [ benchIO "streamly" Streamly.iterateTakeAll Streamly.toNull
      , benchIO "vector" Vector.iterateTakeAll Vector.toNull
      ]
    , bgroup "iterated/dropOne"
      [ benchIO "streamly" Streamly.iterateDropOne Streamly.toNull
      , benchIO "vector" Vector.iterateDropOne Vector.toNull
      ]
    , bgroup "iterated/dropWhileFalse"
      [ benchIO "streamly" Streamly.iterateDropWhileFalse Streamly.toNull
      , benchIO "vector" Vector.iterateDropWhileFalse Vector.toNull
      ]
    , bgroup "iterated/dropWhileTrue"
      [ benchIO "streamly" Streamly.iterateDropWhileTrue Streamly.toNull
      , benchIO "vector" Vector.iterateDropWhileTrue Vector.toNull
      ]
   ]
