import Control.Arrow (second)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Process.Typed (readProcess_)
import Text.CSV (CSV, parseCSVFromFile)

import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as T

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

-------------------------------------------------------------------------------
-- Configurable stuff
-------------------------------------------------------------------------------

outputDir :: String
outputDir = "charts"

packages :: [String]
packages = ["streamly", "streaming", "pipes", "conduit", "machines", "vector"]

-- pairs of benchmark group titles and corresponding list of benchmark
-- prefixes i.e. without the package name at the end.
bmGroups :: [(String, [String])]
bmGroups =
    [
      -- Operations are listed in increasing cost order
      ( "All Operations at a Glance (Shorter is Faster)"
      , [
        -- "filtering/take-one"
          "elimination/toNull"
        , "filtering/drop-all"
        , "elimination/last"
        , "elimination/fold"

        , "filtering/filter-all-out"
        , "filtering/dropWhile-true"
        , "filtering/take-all"
        , "filtering/takeWhile-true"
        , "transformation/map"
        , "filtering/filter-all-in"
        , "filtering/filter-even"

        , "elimination/scan"
        , "transformation/mapM"
        ,  "zip"

        , "elimination/toList"
        , "elimination/concat"
        ]
      )
    , ( "Discarding and Folding (Shorter is Faster)"
      , [
        -- "filtering/take-one"
          "elimination/toNull"
        , "filtering/drop-all"
        , "elimination/last"
        , "elimination/fold"
        ]
      )
    , ( "Pure Transformation and Filtering (Shorter is Faster)"
      , [
          "filtering/filter-all-out"
        , "filtering/dropWhile-true"
        , "filtering/take-all"
        , "filtering/takeWhile-true"
        , "transformation/map"
        , "filtering/filter-all-in"
        , "filtering/filter-even"
        , "elimination/scan"
        ]
      )
    , ( "Monadic Transformation (Shorter is Faster)"
      , [
          "transformation/mapM"
        ]
      )
    , ( "Folding to List (Shorter is Faster)"
      , [
          "elimination/toList"
        ]
      )
    , ( "Zipping and Concating Streams (Shorter is Faster)"
      , [ "zip"
        , "elimination/concat"
        ]
      )
    , ( "Composing Pipeline Stages (Shorter is Faster)"
      , [
          "compose/all-out-filters"
        , "compose/all-in-filters"
        , "compose/map-with-all-in-filter"
        , "compose/mapM"
        ]
      )
    ]

-------------------------------------------------------------------------------

-- "values" has results for each package for each title in bmTitles
genGroupGraph :: String -> [String] -> [(String, [Maybe Double])] -> IO ()
genGroupGraph bmGroupName bmTitles values =
    toFile def (outputDir
                ++ "/"
                -- links in README.rst eat up the space so we match the same
                ++ (filter (not . isSpace) (takeWhile (/= '(') bmGroupName))
                ++ ".svg") $ do
        layout_title .= bmGroupName
        layout_title_style . font_size .= 25
        layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
        layout_x_axis . laxis_style . axis_label_style . font_size .= 12

        -- layout_y_axis . laxis_override .= axisGridAtTicks
        let modifyLabels ad = ad {
                _axis_labels = map (map (second (++ " ms"))) (_axis_labels ad)
            }
        layout_y_axis . laxis_override .= modifyLabels
        -- XXX We are mapping a missing value to 0, can we label it missing
        -- instead?
        let modifyVal x = map ((*1000) . fromMaybe 0) (snd x)
        plot $ fmap plotBars $ bars bmTitles (addIndexes (map modifyVal values))

-- Given a package name (e.g. streaming) and benchmark prefixes (e.g.
-- [elimination/null, elimination/toList]) get the corresponding results e.g.
-- [8.1 ms, 5.4 ms]. The corresponding result file entries will have
-- elimination/null/streaming etc. as the names of the entries.
getResultsForPackage :: CSV -> String -> [String] -> [Maybe Double]
getResultsForPackage csvData pkgname bmPrefixes =
      map (getBenchmarkMean csvData)
    $ map (++ "/" ++ pkgname) bmPrefixes

    where

    getBenchmarkMean entries bmname =
        case filter ((== bmname) .  head) entries of
            [] -> trace
                ("Warning! Benchmark [" ++ bmname ++"] not found in csv data")
                Nothing
            xs -> Just (read ((last xs) !! 1))

genOneGraph :: CSV -> [(String, String)] -> (String, [String]) -> IO ()
genOneGraph csvData pkginfo (bmGroupTitle, prefixes) =
    genGroupGraph bmGroupTitle bmTitles bmResults

    where

    bmTitles = map (last . splitOn "/" ) prefixes

    pkgName = fst
    pkgVersion = snd
    pkgNameWithVersion pkgInfo = pkgName pkgInfo ++ "-" ++ pkgVersion pkgInfo
    pkgGetResults pkgInfo =
        let vals = getResultsForPackage csvData (pkgName pkgInfo) prefixes
        in (pkgNameWithVersion pkgInfo, vals)

    -- this produces results for all packages for all prefixes
    -- [(packagenamewithversion, [Maybe Double])]
    bmResults = map pkgGetResults pkginfo

genGraphs :: CSV -> [(String, String)] -> IO ()
genGraphs csvData pkginfo = mapM_ (genOneGraph csvData pkginfo) bmGroups

-- XXX display GHC version as well
-- XXX display the OS/arch
-- XXX fix the y axis labels
-- XXX fix the legend position
main :: IO ()
main = do
    args <- getArgs

    createDirectoryIfMissing True outputDir

    (out, _) <- readProcess_ "stack --system-ghc list-dependencies --bench"

    -- Get our streaming packages and their versions
    let match [] = Nothing
        match (_ : []) = Nothing
        match (x : y : _) =
            case elem x packages of
                False -> Nothing
                True -> Just (x, y)
        pkginfo =
              catMaybes
            $ map match
            $ map words (lines (T.unpack $ T.decodeUtf8 out))

    -- order them in the order specified in packages so that the order is
    -- can be controlled by the user.
    let pkginfo' = map (\x -> (x, fromJust $ lookup x pkginfo)) packages

    csvData <- parseCSVFromFile (head args)
    case csvData of
        Left e -> error $ show e
        Right dat -> genGraphs dat pkginfo'
    return ()
