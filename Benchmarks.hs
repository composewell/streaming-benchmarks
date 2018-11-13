-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Benchmarks.BenchmarkTH
       (createBgroup, createBgroupN, createBgroupIter, createBgroupIterM)
import Benchmarks.Common (benchIO, benchPure)

import qualified Benchmarks.VectorMonadic as VectorMonadic
import qualified Benchmarks.Streamly as Streamly
import qualified Benchmarks.StreamlyPure as StreamlyPure
import qualified Benchmarks.Streaming as Streaming
import qualified Benchmarks.Machines as Machines
import qualified Benchmarks.Pipes as Pipes
import qualified Benchmarks.Conduit as Conduit
import qualified Benchmarks.Drinkery as Drinkery
import qualified Benchmarks.List as List
import qualified Benchmarks.DList as DList
import qualified Benchmarks.Sequence as Sequence
import qualified Benchmarks.Vector as Vector
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
    , bgroup "transformationX4"
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
    , bgroup "filteringX4"
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
    , bgroup "mixedX4"
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
    , bgroup "appendR[10000]"
      [ benchPure "dlist" DList.appendSourceR DList.toNull
      , benchPure "list" List.appendSourceR List.toNull
      , benchPure "sequence" Sequence.appendSourceR Sequence.toNull
      , benchIO "streamly" Streamly.appendSourceR Streamly.toNull
      , benchPure "streamly-pure" StreamlyPure.appendSourceR
                                  StreamlyPure.toNull
      , benchIO "conduit" Conduit.appendSourceR Conduit.toNull
      -- append benchmark for all these packages shows
      -- quadratic performance slowdown.
      , benchPure "vector" Vector.appendSourceR Vector.toNull
      , benchIO "monadic-vector" VectorMonadic.appendSourceR
                                 VectorMonadic.toNull
      , benchIO "pipes" Pipes.appendSourceR Pipes.toNull
      , benchIO "streaming" Streaming.appendSourceR Streaming.toNull
      ]
    , bgroup "appendL[10000]"
      [ benchPure "dlist" DList.appendSourceL DList.toNull
      , benchPure "sequence" Sequence.appendSourceL Sequence.toNull
      , benchIO   "conduit" Conduit.appendSourceL Conduit.toNull
      -- append benchmark for all these packages shows
      -- quadratic performance slowdown.
      , benchPure "vector" Vector.appendSourceL Vector.toNull
      , benchIO   "monadic-vector" VectorMonadic.appendSourceL
                                   VectorMonadic.toNull
      , benchIO   "streamly" Streamly.appendSourceL Streamly.toNull
      , benchPure "streamly-pure" StreamlyPure.appendSourceL
                                  StreamlyPure.toNull
      , benchPure "list" List.appendSourceL List.toNull
      , benchIO   "pipes" Pipes.appendSourceL Pipes.toNull
      , benchIO   "streaming" Streaming.appendSourceL Streaming.toNull
      ]
      -- Perform 100,000 mapM recursively over a stream of length 10
    , bgroup "iterated"
      [ $(createBgroupIterM "mapM" "iterateMapM")
      , $(createBgroupIter "scan[10000]" "iterateScan")
      , $(createBgroupIter "filterEven" "iterateFilterEven")
      , $(createBgroupIter "takeAll" "iterateTakeAll")
      , $(createBgroupIter "dropOne" "iterateDropOne")
      , $(createBgroupIter "dropWhileFalse[10000]" "iterateDropWhileFalse")
      , $(createBgroupIter "dropWhileTrue" "iterateDropWhileTrue")
      ]
   ]
