-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.DefaultMain (defaultMain) where

import Data.List ((\\))
import Data.Maybe (catMaybes)
import Gauge (bgroup)
import Language.Haskell.TH.Syntax (Q, Exp)

import qualified Gauge as Gauge (defaultMain)

import Benchmarks.BenchmarkTH
import Prelude hiding (all)

all, most, almost :: Select
all = Exclude []
most = Exclude ["DList"]
almost = Exclude ["DList", "Sequence"]

defaultMain :: String -> Q Exp
defaultMain name = [| do
  Gauge.defaultMain [ bgroup name $ catMaybes
    [ Just $ bgroup "elimination" $ catMaybes
      [ $(createBgroupSink all name "drain" "toNull")
      , $(createBgroupSink (Exclude ["List"]) name "toList" "toList")
      , $(createBgroupSink all name "foldl'" "foldl")
      , $(createBgroupSink most name "last" "last")
      , $(createBgroupSrc (Include ["Streamly", "List", "VectorStreams"])
            name "enumInt" "sourceIntFromThenTo")
      ]
    , Just $ bgroup "transformation" $ catMaybes
      [ $(createBgroupSinkN almost name "scan" "scan" 1)
      , $(createBgroupSinkN all name "map" "map" 1)
      , $(createBgroupSinkN most name "mapM" "mapM" 1)
      ]
    , Just $ bgroup "transformationX4" $ catMaybes
      [ $(createBgroupSinkN almost name "scan x 4" "scan" 4)
      , $(createBgroupSinkN all name "map x 4" "map" 4)
      , $(createBgroupSinkN most name "mapM x 4" "mapM" 4)
      ]
    , Just $ bgroup "filtering" $ catMaybes
      [ $(createBgroupSinkN most name "filter-all-out" "filterAllOut" 1)
      , $(createBgroupSinkN most name "filter-all-in" "filterAllIn" 1)
      , $(createBgroupSinkN most name "drop-all" "dropAll" 1)
      , $(createBgroupSinkN most name "takeWhile-true" "takeWhileTrue" 1)
      , $(createBgroupSinkN most name "filter-even" "filterEven" 1)
      , $(createBgroupSinkN most name "take-all" "takeAll" 1)
      , $(createBgroupSinkN most name "drop-one" "dropOne" 1)
      , $(createBgroupSinkN most name "dropWhile-true" "dropWhileTrue" 1)
      , $(createBgroupSinkN most name "dropWhile-false" "dropWhileFalse" 1)
      ]
      -- drop-all/dropWhile-true/filter-all-out x 4 would be the same as
      -- drop-all/dropWhile-true/filter-all-out x 1 as they would already
      -- drop all elements and do nothing in the next iterations
    , Just $ bgroup "filteringX4" $ catMaybes
      [ $(createBgroupSinkN most name "filter-even x 4" "filterEven" 4)
      -- , $(createBgroupSinkN most name "filter-all-out x 4" "filterAllOut" 4)
      , $(createBgroupSinkN most name "filter-all-in x 4" "filterAllIn" 4)
      , $(createBgroupSinkN most name "take-all x 4" "takeAll" 4)
      , $(createBgroupSinkN most name "takeWhile-true x 4" "takeWhileTrue" 4)
      , $(createBgroupSinkN most name "drop-one x 4" "dropOne" 4)
      -- , $(createBgroupSinkN most name "drop-all x 4" "dropAll" 4)
      -- , $(createBgroupSinkN most name "dropWhile-true x 4" "dropWhileTrue" 4)
      , $(createBgroupSinkN most name "dropWhile-false x 4" "dropWhileFalse" 4)
      ]
    , Just $ bgroup "mixedX4" $ catMaybes
      [ $(createBgroupSinkN almost name "scan-map x 4" "scanMap" 4)
      , $(createBgroupSinkN almost name "drop-scan x 4" "dropScan" 4)
      , $(createBgroupSinkN almost name "take-scan x 4" "takeScan" 4)
      , $(createBgroupSinkN almost name "filter-scan x 4" "filterScan" 4)
      , $(createBgroupSinkN most name "drop-map x 4" "dropMap" 4)
      , $(createBgroupSinkN most name "take-drop x 4" "takeDrop" 4)
      , $(createBgroupSinkN most name "take-map x 4" "takeMap" 4)
      , $(createBgroupSinkN most name "filter-drop x 4" "filterDrop" 4)
      , $(createBgroupSinkN most name "filter-take x 4" "filterTake" 4)
      , $(createBgroupSinkN most name "filter-map x 4" "filterMap" 4)
      ]
    , $(createBgroupSink (Exclude ["DList", "StreamlyArray"]) name "zip" "zip")
    -- XXX use 4x250k concatMap for a comparative idea of cost wrt other ops
    , $(createBgroupSink (Include
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
        ]) name "concatMap" "concatMap")
    , $(createBgroupSink (Exclude
        [ "List"
        , "DList"
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
        ]) name "concatMapFoldable" "concatMapFoldable")

    , $(createBgroupSrc (Exclude
        [ "Drinkery"
        , "StreamlyArray"
        ]) name "appendR[10000]" "appendSourceR")
    , $(createBgroupSrc (Include
        [ "DList"
        , "Conduit"
        ]) name "appendL[10000]" "appendSourceL")
      -- Perform 100,000 mapM recursively over a stream of length 10
    , Just $ bgroup "iterated" $ catMaybes
      [ $(createBgroupSrc (Include (iterMods \\ pureMods)) name
            "mapM (iter)" "iterateMapM")
      , $(createBgroupSrc (Include (iterMods \\ ["Sequence"])) name
            "scan[10000] (iter)" "iterateScan")
      , $(createBgroupSrc (Include iterMods) name
            "filterEven (iter)" "iterateFilterEven")
      , $(createBgroupSrc (Include iterMods) name
            "takeAll (iter)" "iterateTakeAll")
      , $(createBgroupSrc (Include iterMods) name
            "dropOne (iter)" "iterateDropOne")
      , $(createBgroupSrc (Include iterMods) name
            "dropWhileFalse[10000] (iter)" "iterateDropWhileFalse")
      , $(createBgroupSrc (Include iterMods) name
            "dropWhileTrue (iter)" "iterateDropWhileTrue")
      ]
   ]
   ]
   |]
