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
    ( "Elimination Operations"
      , [
          "elimination/drain"
        , "elimination/last"
        , "elimination/fold"
        ]
      )
    , ( "Transformation Operations"
      , [ "transformation/map"
        , "transformation/mapM"
        , "transformation/scan"
        ]
      )
    , ( "Transformation Operations x 4"
      , [ "transformationN/map"
        , "transformationN/mapM"
        , "transformationN/scan"
        ]
      )
    , ( "Filtering  Operations"
      , [
          "filtering/filter-all-out"
        , "filtering/filter-all-in"
        , "filtering/drop-all"
        , "filtering/takeWhile-true"
        , "filtering/take-all"
        , "filtering/dropWhile-true"
        , "filtering/filter-even"
        , "filtering/drop-one"
        , "filtering/dropWhile-false"
        ]
      )
    , ( "Filtering  Operations x 4"
      , [
          "filteringN/filter-all-out"
        , "filteringN/takeWhile-true"
        , "filteringN/filter-all-in"
        , "filteringN/take-all"
        , "filteringN/filter-even"
        , "filteringN/drop-all"
        , "filteringN/dropWhile-true"
        , "filteringN/dropWhile-false"
        , "filteringN/drop-one"
        ]
      )
    , ( "Composed Operations x 4"
      , [ "composed/filter-map"
        , "composed/take-map"
        , "composed/drop-map"
        , "composed/filter-drop"
        , "composed/filter-take"
        , "composed/take-drop"
        , "composed/scan-map"
        , "composed/filter-scan"
        , "composed/take-scan"
        , "composed/drop-scan"
        ]
      )
    , ( "Append Operation"
      , [ "append"
        ]
      )
    , ( "Zip Operation"
      , [ "zip"
        ]
      )
    , ( "Concat Operation"
      , [ "concat"
        ]
      )
    , ( "Conversion Operations"
      , [ "elimination/toList"
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

    putStrLn "Creating time charts..."
    mapM_ (makeOneGraph input "time") charts
    -- allocation charts are not very interesting
    -- putStrLn "\nCreating allocation charts..."
    -- mapM_ (makeOneGraph input "allocated") charts
    putStrLn "\nCreating maxrss charts..."
    mapM_ (makeOneGraph input "maxrss") charts

-- Pass <input file> <comma separated list of packages> <True/False>
main :: IO ()
main = withCli createCharts
