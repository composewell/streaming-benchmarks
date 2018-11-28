-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.List ((\\))
import Gauge

import Benchmarks.BenchmarkTH

import qualified Benchmarks.VectorMonadic as VectorMonadic
import qualified Benchmarks.Streamly as Streamly
import qualified Benchmarks.StreamlyPure as StreamlyPure
import qualified Benchmarks.Streaming as Streaming
import qualified Benchmarks.Machines as Machines
import qualified Benchmarks.Pipes as Pipes
import qualified Benchmarks.Conduit as Conduit
import qualified Benchmarks.Drinkery as Drinkery
import qualified Benchmarks.List as List
import qualified Benchmarks.ByteString as ByteString
import qualified Benchmarks.Text as Text
import qualified Benchmarks.DList as DList
import qualified Benchmarks.Sequence as Sequence
import qualified Benchmarks.Vector as Vector
-- import qualified Benchmarks.LogicT as LogicT
-- import qualified Benchmarks.ListT as ListT
-- import qualified Benchmarks.ListTransformer as ListTransformer

main :: IO ()
main = do
  defaultMain
    [ bgroup "elimination"
      [ $(createBgroupSink (benchMods ++ ["DList"]) "drain" "toNull")
      , $(createBgroupSink (benchMods ++ ["DList"]) "toList" "toList")
      , $(createBgroupSink (benchMods ++ ["DList"]) "foldl'" "foldl")
      , $(createBgroupSink benchMods "last" "last")
      , $(createBgroupSrc (["Streamly", "List", "VectorMonadic"])
            "enumInt" "sourceIntFromThenTo")
      ]
    , bgroup "transformation"
      [ $(createBgroupSinkN (benchMods \\ ["Sequence"]) "scan" "scan" 1)
      , $(createBgroupSinkN (benchMods ++ ["DList"]) "map" "map" 1)
      , $(createBgroupSinkN benchMods "mapM" "mapM" 1)
      ]
    , bgroup "transformationX4"
      [ $(createBgroupSinkN (benchMods \\ ["Sequence"]) "scan" "scan" 4)
      , $(createBgroupSinkN (benchMods ++ ["DList"]) "map" "map" 4)
      , $(createBgroupSinkN benchMods "mapM" "mapM" 4)
      ]
    , bgroup "filtering"
      [ $(createBgroupSinkN benchMods "filter-all-out" "filterAllOut" 1)
      , $(createBgroupSinkN benchMods "filter-all-in" "filterAllIn" 1)
      , $(createBgroupSinkN benchMods "drop-all" "dropAll" 1)
      , $(createBgroupSinkN benchMods "takeWhile-true" "takeWhileTrue" 1)
      , $(createBgroupSinkN benchMods "filter-even" "filterEven" 1)
      , $(createBgroupSinkN benchMods "take-all" "takeAll" 1)
      , $(createBgroupSinkN benchMods "drop-one" "dropOne" 1)
      , $(createBgroupSinkN benchMods "dropWhile-true" "dropWhileTrue" 1)
      , $(createBgroupSinkN benchMods "dropWhile-false" "dropWhileFalse" 1)
      ]
    , bgroup "filteringX4"
      [ $(createBgroupSinkN benchMods "filter-even" "filterEven" 4)
      , $(createBgroupSinkN benchMods "filter-all-out" "filterAllOut" 4)
      , $(createBgroupSinkN benchMods "filter-all-in" "filterAllIn" 4)
      , $(createBgroupSinkN benchMods "take-all" "takeAll" 4)
      , $(createBgroupSinkN benchMods "takeWhile-true" "takeWhileTrue" 4)
      , $(createBgroupSinkN benchMods "drop-one" "dropOne" 4)
      , $(createBgroupSinkN benchMods "drop-all" "dropAll" 4)
      , $(createBgroupSinkN benchMods "dropWhile-true" "dropWhileTrue" 4)
      , $(createBgroupSinkN benchMods "dropWhile-false" "dropWhileFalse" 4)
      ]
    , bgroup "mixedX4"
      [ $(createBgroupSinkN (benchMods \\ ["Sequence"]) "scan-map" "scanMap" 4)
      , $(createBgroupSinkN (benchMods \\ ["Sequence"]) "drop-scan" "dropScan" 4)
      , $(createBgroupSinkN (benchMods \\ ["Sequence"]) "take-scan" "takeScan" 4)
      , $(createBgroupSinkN (benchMods \\ ["Sequence"]) "filter-scan" "filterScan" 4)
      , $(createBgroupSinkN benchMods "drop-map" "dropMap" 4)
      , $(createBgroupSinkN benchMods "take-drop" "takeDrop" 4)
      , $(createBgroupSinkN benchMods "take-map" "takeMap" 4)
      , $(createBgroupSinkN benchMods "filter-drop" "filterDrop" 4)
      , $(createBgroupSinkN benchMods "filter-take" "filterTake" 4)
      , $(createBgroupSinkN benchMods "filter-map" "filterMap" 4)
      ]
    , $(createBgroupSink benchMods "zip" "zip")
    , $(createBgroupSink (benchMods \\ ["Streamly", "Sequence"])
                         "concat" "concat")

    , $(createBgroupSrc ((benchMods ++ ["DList"]) \\ ["Drinkery"])
                        "appendR[10000]" "appendSourceR")
    , $(createBgroupSrc ((benchMods ++ ["DList"]) \\ ["Drinkery"])
                        "appendL[10000]" "appendSourceL")

      -- Perform 100,000 mapM recursively over a stream of length 10
    , bgroup "iterated"
      [ $(createBgroupSrc (iterMods \\ pureMods) "mapM" "iterateMapM")
      , $(createBgroupSrc (iterMods \\ ["Sequence"]) "scan[10000]" "iterateScan")
      , $(createBgroupSrc iterMods "filterEven" "iterateFilterEven")
      , $(createBgroupSrc iterMods "takeAll" "iterateTakeAll")
      , $(createBgroupSrc iterMods "dropOne" "iterateDropOne")
      , $(createBgroupSrc iterMods "dropWhileFalse[10000]" "iterateDropWhileFalse")
      , $(createBgroupSrc iterMods "dropWhileTrue" "iterateDropWhileTrue")
      ]
   ]
