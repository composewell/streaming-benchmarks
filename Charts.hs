{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch, ErrorCall(..))
import Data.Char (isSpace)
import Data.List (reverse, sortOn, isPrefixOf)
-- import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
-- import Data.Maybe (fromJust)
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
    ( "Elimination Operations"
      , [
          "elimination/drain"
        , "elimination/last"
        , "elimination/foldl'"
        ]
      )
    , ( "Transformation Operations"
      , [ "transformation/map"
        , "transformation/mapM"
        , "transformation/scan"
        ]
      )
    , ( "Transformation Operations x 4"
      , [ "transformationX4/map x 4"
        , "transformationX4/mapM x 4"
        , "transformationX4/scan x 4"
        ]
      )
    , ( "Filtering Operations"
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
    , ( "Filtering Operations x 4"
      , [
          "filteringX4/filter-all-out x 4"
        , "filteringX4/takeWhile-true x 4"
        , "filteringX4/filter-all-in x 4"
        , "filteringX4/take-all x 4"
        , "filteringX4/filter-even x 4"
        , "filteringX4/drop-all x 4"
        , "filteringX4/dropWhile-true x 4"
        , "filteringX4/dropWhile-false x 4"
        , "filteringX4/drop-one x 4"
        ]
      )
    , ( "Mixed Operations x 4"
      , [ "mixedX4/filter-map x 4"
        , "mixedX4/take-map x 4"
        , "mixedX4/drop-map x 4"
        , "mixedX4/filter-drop x 4"
        , "mixedX4/filter-take x 4"
        , "mixedX4/take-drop x 4"
        , "mixedX4/scan-map x 4"
        , "mixedX4/filter-scan x 4"
        , "mixedX4/take-scan x 4"
        , "mixedX4/drop-scan x 4"
        ]
      )
    , ( "Iterated Operations"
      , [ "iterated/mapM"
        , "iterated/scan[x0.01]"
        , "iterated/filterEven"
        , "iterated/takeAll"
        , "iterated/dropOne"
        , "iterated/dropWhileFalse[x0.01]"
        , "iterated/dropWhileTrue"
        ]
      )
    , ( "Append Operations"
      , [ "appendR"
      --  , "appendL"
        ]
      )
    , ( "Zip Operation"
      , [ "zip"
        ]
      )
    , ( "Concat Operation"
      , [ "concatMap"
        , "concatMapFoldable"
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
    (ecode, out, _) <- readProcess "stack list-dependencies --bench"

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

ignoringErr :: IO () -> IO ()
ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" ++ err ++ "\nSkipping.")

createCharts :: String -> String -> Bool -> String -> Bool -> IO ()
createCharts input pkgList graphs delta versions = do
    let packages = splitOn "," pkgList

    pkgInfo <-
        if versions
        then getPkgVersions packages
        else return []

    let cmpStyle = case delta of
            "absolute" -> Absolute
            "multiples" -> Multiples
            "percent" -> PercentDiff
            x -> error $ "Unknown compare option: " ++ show x

    let bsort pxs bs =
                let i = intersect (map (last . splitOn "/") pxs) bs
                in i ++ (bs \\ i)
    let cfg (t, prefixes) = defaultConfig
            { mkTitle = Just t
            , outputDir = Just "charts"
            , presentation = Groups cmpStyle
            , diffStrategy = SingleEstimator
            , omitBaseline = True
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
                in nub $ concat $ map (\x -> filter (\(y,_) -> y == x) gs) new
            , selectBenchmarks = \g -> bsort prefixes $
                either error (map fst) $ g (ColumnIndex 0) Nothing
            }

        -- links in README.rst eat up the space so we match the same
    let toOutfile t = filter (not . isSpace) (takeWhile (/= '(') t)
        packages' = packages
    {-
    let packages' = map (\x ->
            if "pure-" `isPrefixOf` x
            then fromJust (stripPrefix "pure-" x)
            else x) packages

    let selectByRegression f =
            reverse
          $ fmap fst
          $ either
              (const $ either error id $ f (ColumnIndex 0) Nothing)
              (sortOn snd)
              $ f (ColumnIndex 1) Nothing

    let makeOneGraph infile (t, prefixes) = do
            let title' fname =
                        if delta /= "absolute"
                        then t ++ " (" ++ fname ++ ") relative to " ++ packages' !! 0
                        else t ++ " (" ++ fname ++ ") (Lower is Better)"
                cfg' = cfg (title', prefixes)
                cfg'' =
                    if delta /= "absolute"
                    then cfg' { selectBenchmarks = selectByRegression }
                    else cfg'
            if graphs
            then ignoringErr $ graph infile (toOutfile t) cfg''
            else ignoringErr $ report infile Nothing cfg''

    mapM_ (makeOneGraph input) charts
    -}

    let cutOffByRegression p f =
            let cmp = if delta == "absolute"
                      then Multiples
                      else cmpStyle
            in reverse
              $ fmap fst
              $ filter (\(_,y) -> p y) . (sortOn snd)
              $ either
                  (const $ either error id $ f (ColumnIndex 0) (Just cmp))
                  id
                  $ f (ColumnIndex 1) (Just cmp)

    -- Make a graph of all operations sorted based on performance regression in
    -- descending order and operations below a 10% threshold filtered out.
    let makeDiffGraph infile prefixes t p = do
            let cfg' = (cfg (t, prefixes))
                    { presentation = Groups cmpStyle
                    , selectBenchmarks = cutOffByRegression p
                    }
            if graphs
            then ignoringErr $ graph infile (toOutfile (t "")) cfg'
            else ignoringErr $ report infile Nothing cfg'

    -- compare two packages for best and worst operations
    let makeTitle fname =
            if delta /= "absolute"
            then
                let prefix = if delta == "percent"
                             then "Extra % "
                             else ""
                    connector = if delta == "multiples"
                             then "as multiples of"
                             else "wrt"
                    field = case fname of
                        "time" -> "time taken"
                        "maxrss" -> "memory used"
                        x -> x
                    pkg = if length packages' == 2
                          then "by '" ++ packages' !! 1 ++ "'"
                          else ""
                in prefix ++ field ++ " " ++ pkg ++ " " ++ connector
                    ++ " '" ++ packages' !! 0 ++ "'"
            else fname ++ " (Lower is Better)"
    let p x =
            case delta of
                "absolute" -> True
                "multiples" -> x < (-1.1) || x > 1.1
                "percent" -> x < (-10) || x > 10
                y -> error $ "Unknown compare option: " ++ show y
    makeDiffGraph input (concatMap snd charts) makeTitle p

-- Pass <input file> <comma separated list of packages> <True/False>
main :: IO ()
main = withCli createCharts
