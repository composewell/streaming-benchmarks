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

import qualified Benchmarks.VectorStreams as VectorStreams
import qualified Benchmarks.Streamly as Streamly
import qualified Benchmarks.StreamlyPure as StreamlyPure
import qualified Benchmarks.Streaming as Streaming
import qualified Benchmarks.Machines as Machines
import qualified Benchmarks.Pipes as Pipes
import qualified Benchmarks.Conduit as Conduit
import qualified Benchmarks.Drinkery as Drinkery
import qualified Benchmarks.List as List
import qualified Benchmarks.ByteString as ByteString
import qualified Benchmarks.ByteStringLazy as ByteStringLazy
import qualified Benchmarks.Text as Text
import qualified Benchmarks.DList as DList
import qualified Benchmarks.Sequence as Sequence
import qualified Benchmarks.Vector as Vector
import qualified Benchmarks.VectorUnboxed as VectorUnboxed
import qualified Benchmarks.VectorStorable as VectorStorable
import qualified Benchmarks.StreamlyArray as StreamlyArray
-- import qualified Benchmarks.LogicT as LogicT
-- import qualified Benchmarks.ListT as ListT
-- import qualified Benchmarks.ListTransformer as ListTransformer

main :: IO ()
main = do
  defaultMain
    [ bgroup "elimination"
      [ $(createBgroupSink (benchMods ++ ["DList"]) "drain" "toNull")
      , $(createBgroupSink ((benchMods \\ ["List"]) ++ ["DList"]) "toList" "toList")
      , $(createBgroupSink (benchMods ++ ["DList"]) "foldl'" "foldl")
      , $(createBgroupSink benchMods "last" "last")
      , $(createBgroupSrc (["Streamly", "List", "VectorStreams"])
            "enumInt" "sourceIntFromThenTo")
      ]
    , bgroup "transformation"
      [ $(createBgroupSinkN (benchMods \\ ["Sequence"]) "scan" "scan" 1)
      , $(createBgroupSinkN (benchMods ++ ["DList"]) "map" "map" 1)
      , $(createBgroupSinkN benchMods "mapM" "mapM" 1)
      ]
    , bgroup "transformationX4"
      [ $(createBgroupSinkN (benchMods \\ ["Sequence"]) "scan x 4" "scan" 4)
      , $(createBgroupSinkN (benchMods ++ ["DList"]) "map x 4" "map" 4)
      , $(createBgroupSinkN benchMods "mapM x 4" "mapM" 4)
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
      -- drop-all/dropWhile-true/filter-all-out x 4 would be the same as
      -- drop-all/dropWhile-true/filter-all-out x 1 as they would already
      -- drop all elements and do nothing in the next iterations
    , bgroup "filteringX4"
      [ $(createBgroupSinkN benchMods "filter-even x 4" "filterEven" 4)
      -- , $(createBgroupSinkN benchMods "filter-all-out x 4" "filterAllOut" 4)
      , $(createBgroupSinkN benchMods "filter-all-in x 4" "filterAllIn" 4)
      , $(createBgroupSinkN benchMods "take-all x 4" "takeAll" 4)
      , $(createBgroupSinkN benchMods "takeWhile-true x 4" "takeWhileTrue" 4)
      , $(createBgroupSinkN benchMods "drop-one x 4" "dropOne" 4)
      -- , $(createBgroupSinkN benchMods "drop-all x 4" "dropAll" 4)
      -- , $(createBgroupSinkN benchMods "dropWhile-true x 4" "dropWhileTrue" 4)
      , $(createBgroupSinkN benchMods "dropWhile-false x 4" "dropWhileFalse" 4)
      ]
    , bgroup "mixedX4"
      [ $(createBgroupSinkN (benchMods \\ ["Sequence"]) "scan-map x 4" "scanMap" 4)
      , $(createBgroupSinkN (benchMods \\ ["Sequence"]) "drop-scan x 4" "dropScan" 4)
      , $(createBgroupSinkN (benchMods \\ ["Sequence"]) "take-scan x 4" "takeScan" 4)
      , $(createBgroupSinkN (benchMods \\ ["Sequence"]) "filter-scan x 4" "filterScan" 4)
      , $(createBgroupSinkN benchMods "drop-map x 4" "dropMap" 4)
      , $(createBgroupSinkN benchMods "take-drop x 4" "takeDrop" 4)
      , $(createBgroupSinkN benchMods "take-map x 4" "takeMap" 4)
      , $(createBgroupSinkN benchMods "filter-drop x 4" "filterDrop" 4)
      , $(createBgroupSinkN benchMods "filter-take x 4" "filterTake" 4)
      , $(createBgroupSinkN benchMods "filter-map x 4" "filterMap" 4)
      ]
    , $(createBgroupSink (benchMods \\ ["StreamlyArray", "StreamlyPure"]) "zip" "zip")
    -- XXX use 4x250k concatMap for a comparative idea of cost wrt other ops
    , $(createBgroupSink (
        [ "List"
        , "Streamly"
        , "StreamlyPure"
        , "VectorStreams"
        , "Vector"
        , "VectorStorable"
        , "VectorUnboxed"
        , "ByteString"
        , "ByteStringLazy"
        , "Text"
        ]) "concatMap" "concatMap")
    , $(createBgroupSink (benchMods \\
            [ "List"
            , "Streamly"
            , "StreamlyPure"
            , "VectorStreams"
            , "Vector"
            , "VectorStorable"
            , "VectorUnboxed"
            , "ByteString"
            , "ByteStringLazy"
            , "Sequence"
            , "StreamlyArray"
            , "Text"
            ]) "concatMapFoldable" "concatMapFoldable")

    , $(createBgroupSrc ((benchMods ++ ["DList"]) \\
            ["Drinkery", "StreamlyArray"]) "appendR[10000]" "appendSourceR")
    , $(createBgroupSrc ["DList", "Conduit"] "appendL[10000]" "appendSourceL")

      -- Perform 100,000 mapM recursively over a stream of length 10
    , bgroup "iterated"
      [ $(createBgroupSrc (iterMods \\ pureMods) "mapM (iter)" "iterateMapM")
      , $(createBgroupSrc (iterMods \\ ["Sequence"]) "scan[10000] (iter)" "iterateScan")
      , $(createBgroupSrc iterMods "filterEven (iter)" "iterateFilterEven")
      , $(createBgroupSrc iterMods "takeAll (iter)" "iterateTakeAll")
      , $(createBgroupSrc iterMods "dropOne (iter)" "iterateDropOne")
      , $(createBgroupSrc iterMods "dropWhileFalse[10000] (iter)" "iterateDropWhileFalse")
      , $(createBgroupSrc iterMods "dropWhileTrue (iter)" "iterateDropWhileTrue")
      ]
   ]
