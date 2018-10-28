{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (reverse, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import System.Process.Typed (readProcess)
import BenchShow
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

createCharts :: String -> String -> Bool -> Bool -> IO ()
createCharts input pkgList graphs delta = do
    let packages = splitOn "," pkgList
    let pkgInfo = []
    -- pkgInfo <- getPkgVersions
        bsort pxs bs =
                let i = intersect (map (last . splitOn "/") pxs) bs
                in i ++ (bs \\ i)
        selectByRegression f =
            reverse
          $ fmap fst
          $ either
              (const $ either error id $ f $ ColumnIndex 0)
              (sortOn snd)
              $ f $ ColumnIndex 1

    let cfg (t, prefixes) = defaultConfig
            { title = Just t
            , outputDir = Just "charts"
            , presentation =
                if delta then Groups PercentDiff else Groups Absolute
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
            , selectGroups = \gs ->
                let gs' = map fst gs
                    i = intersect (map (suffixVersion pkgInfo) packages) gs'
                    new = i ++ (gs' \\ i)
                in concat $ map (\x -> filter (\(y,_) -> y == x) gs) new
            , selectBenchmarks = \g -> bsort prefixes $
                either error (map fst) $ g (ColumnIndex 0)
            }

        -- links in README.rst eat up the space so we match the same
        toOutfile t = filter (not . isSpace) (takeWhile (/= '(') t)

        makeOneGraph infile (t, prefixes) = do
            let title' = t ++ " (Lower is Better)"
                cfg' = cfg (t, prefixes)
                cfg'' =
                    if delta
                    then cfg' { selectBenchmarks = selectByRegression }
                    else cfg'
            report infile Nothing cfg''
            when graphs $ graph infile (toOutfile t) cfg''

    putStrLn "Creating time charts..."
    mapM_ (makeOneGraph input) charts

-- Pass <input file> <comma separated list of packages> <True/False>
main :: IO ()
main = withCli createCharts
