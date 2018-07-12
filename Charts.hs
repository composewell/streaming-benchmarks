{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.Process.Typed (readProcess_)
import BenchGraph (bgraph, defaultConfig, Config(..))

import Data.List

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

-- XXX use package name and a tag
packages :: [String]
packages =
    [ "list"
    , "pure-vector"
    , "vector"
    , "streamly"
    , "streaming"
    , "pipes"
    , "conduit"
    , "machines"
    , "drinkery"
    ]

-- pairs of benchmark group titles and corresponding list of benchmark
-- prefixes i.e. without the package name at the end.
charts :: [(String, [String])]
charts =
    [
      -- Operations are listed in increasing cost order
      ( "Summary (Shorter is Faster)"
      , [
          "elimination/drain"
        , "transformation/mapM"
--        , "append"
        , "zip"
        ]
      )

    , ( "Detailed (Shorter is Faster)"
      , [
          "elimination/drain"
        , "filtering/drop-all"
      --  , "filtering/dropWhile-true"
      --  , "filtering/filter-all-out"
        , "elimination/last"
        , "elimination/fold"
        -- "filtering/take-one"
        , "transformation/map"
        , "filtering/take-all"
        --, "filtering/takeWhile-true"
        -- , "filtering/filter-all-in"
        , "filtering/filter-even"
        , "transformation/scan"
        , "transformation/mapM"
        -- , "append"
        , "zip"
        -- , "transformation/concat"
        , "elimination/toList"
        ]
      )

    , ( "Composed Ops 4 times (Shorter is Faster)"
      , [ "compose/filter-even"
        -- , "compose/all-in-filters"
        -- , "compose/all-in-filters"
        , "compose/mapM"
        --, "compose/map-with-all-in-filter"
        , "compose/map-and-filter"
        ]
      )
    ]

-------------------------------------------------------------------------------
main :: IO ()
main = do
    (out, _) <- readProcess_ "stack --system-ghc list-dependencies --bench"

    -- Get our streaming packages and their versions
    let match [] = Nothing
        match (_ : []) = Nothing
        match (x : y : _) =
            case elem x packages of
                False -> Nothing
                True -> Just (x, y)

        -- pkginfo is [(packagename, version)]
        pkginfo =
              catMaybes
            $ map match
            $ map words (lines (T.unpack $ T.decodeUtf8 out))

    -- suffix versions to packages
    let suffixVersion p =
            case lookup p pkginfo of
                Nothing -> p
                Just v -> p ++ "-" ++ v

        cfg (title, prefixes) = defaultConfig
            { chartTitle = Just title
            , outputDir = "charts"
            , classifyBenchmark = \bm ->
                case any (`isPrefixOf` bm) prefixes of
                    True ->
                        let xs = reverse (splitOn "/" bm)
                        in Just (suffixVersion (xs !! 0), xs !! 1)
                    False -> Nothing
            , sortBenchmarks = \bs ->
                let i = intersect (map (last . splitOn "/") prefixes) bs
                in i ++ (bs \\ i)
            , sortBenchGroups = \gs ->
                let i = intersect (map suffixVersion packages) gs
                in i ++ (gs \\ i)
            }

    -- links in README.rst eat up the space so we match the same
    let toOutfile title field =
               (filter (not . isSpace) (takeWhile (/= '(') title))
            ++ "-"
            ++ field

        makeOneGraph infile field desc@(title, _) =
            bgraph infile (toOutfile title field) field (cfg desc)

    input <- fmap head getArgs
    mapM_ (makeOneGraph input "Time") charts
    -- mapM_ (makeOneGraph input "mean") charts
    -- mapM_ (makeOneGraph "allocated") charts
