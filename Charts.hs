{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import System.Process.Typed (readProcess)
import BenchGraph (bgraph, defaultConfig, Config(..), ComparisonStyle(..))
import WithCli (withCli)

import Data.List

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

-- pairs of benchmark group titles and corresponding list of benchmark
-- prefixes i.e. without the package name at the end.
charts :: [(String, [String])]
charts =
    [
      -- Operations are listed in increasing cost order
      {-
      ( "Key Operations"
      , [
          "elimination/fold"
        , "transformation/mapM"
        , "filtering/filter-even"
        , "zip"
        ]
      )
    , -} ( "Append Operation"
      , [ "append"
        ]
      )
    , ( "Key Operations"
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
        , "zip"
        -- , "transformation/concat"
        ]
      )
    , ( "toList Operation"
      , [ "elimination/toList"
        ]
      )
    , ( "Composed Operations: 4 times"
      , [ "compose/mapM"
        , "compose/all-in-filters"
        , "compose/map-with-all-in-filter"
        ]
      )
    ]

-------------------------------------------------------------------------------

-- returns [(packagename, version)]
getPkgVersions :: [String] -> IO [(String, String)]
getPkgVersions packages = do
    (ecode, out, _) <- readProcess "stack --system-ghc list-dependencies --bench"

    case ecode of
        ExitSuccess -> do
            -- Get our streaming packages and their versions
            let match [] = Nothing
                match (_ : []) = Nothing
                match (x : y : _) =
                    case elem x packages of
                        False -> Nothing
                        True -> Just (x, y)

             in return
                $ catMaybes
                $ map match
                $ map words (lines (T.unpack $ T.decodeUtf8 out))
        ExitFailure _ -> do
            putStrLn $ "Warning! Cannot determine package versions, "
                ++ "the 'stack list-dependencies' command failed."
            return []

-- suffix versions to packages
suffixVersion :: [(String, String)] -> String -> String
suffixVersion pkginfo p =
    case lookup p pkginfo of
        Nothing -> p
        Just v -> p ++ "-" ++ v

createCharts :: String -> String -> Bool -> IO ()
createCharts input pkgList delta = do
    let packages = splitOn "," pkgList
    let pkgInfo = []
    -- pkgInfo <- getPkgVersions
    let cfg (title, prefixes) = defaultConfig
            { chartTitle = Just title
            , outputDir = "charts"
            , comparisonStyle = if delta then CompareDelta else CompareFull
            , classifyBenchmark = \bm ->
                case any (`isPrefixOf` bm) prefixes of
                    True ->
                        let xs = reverse (splitOn "/" bm)
                            grp   = xs !! 0
                            bench = xs !! 1
                        in case grp `elem` packages of
                                True -> Just (suffixVersion pkgInfo grp, bench)
                                False -> Nothing
                    False -> Nothing
            , sortBenchmarks = \bs ->
                    let i = intersect (map (last . splitOn "/") prefixes) bs
                    in i ++ (bs \\ i)
            , sortBenchGroups = \gs ->
                    let i = intersect (map (suffixVersion pkgInfo) packages) gs
                    in i ++ (gs \\ i)
            }

    -- links in README.rst eat up the space so we match the same
    let toOutfile title field =
               (filter (not . isSpace) (takeWhile (/= '(') title))
            ++ "-"
            ++ field

        makeOneGraph infile field (title, prefixes) = do
            let title' =
                       title
                    ++ " (" ++ field ++ ")"
                    ++ " (Lower is Better)"
            bgraph infile (toOutfile title field) field (cfg (title', prefixes))

    mapM_ (makeOneGraph input "time") charts
    mapM_ (makeOneGraph input "allocated") charts
    mapM_ (makeOneGraph input "maxrss") charts

-- Pass <input file> <comma separated list of packages> <True/False>
main :: IO ()
main = withCli createCharts
