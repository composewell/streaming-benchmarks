{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow (second)
import Data.Char (isSpace)
import Data.List (nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, catMaybes, fromMaybe)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Process.Typed (readProcess_)
import Text.CSV (CSV, parseCSVFromFile)
import Control.Monad.Trans.State.Lazy (get, put)

import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as T

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

-------------------------------------------------------------------------------
-- Configurable stuff
-------------------------------------------------------------------------------

outputDir :: String
outputDir = "charts"

-- XXX use package name and a tag
packages :: [String]
packages = ["list", "pure-vector", "streamly", "vector", "streaming", "pipes", "conduit", "machines", "drinkery"]

-- pairs of benchmark group titles and corresponding list of benchmark
-- prefixes i.e. without the package name at the end.
bmGroups :: [(String, [String])]
bmGroups =
    [
      -- Operations are listed in increasing cost order
      ( "Cheaper Operations (Shorter is Faster)"
      , [
          "elimination/drain"
        , "filtering/drop-all"
        , "filtering/dropWhile-true"
        , "filtering/filter-all-out"
        , "elimination/last"
        , "elimination/fold"
        -- "filtering/take-one"
        , "transformation/map"
        , "filtering/take-all"
        , "filtering/takeWhile-true"
        , "filtering/filter-all-in"
        , "filtering/filter-even"
        , "transformation/scan"
        ]
      )

    , ( "Expensive operations (Shorter is Faster)"
      , [ "transformation/mapM"
        , "append"
        , "zip"
        , "transformation/concat"
        , "elimination/toList"
        ]
      )
      {-
    , ( "Discarding and Folding (Shorter is Faster)"
      , [
        -- "filtering/take-one"
          "elimination/toNull"
        , "filtering/drop-all"
        , "elimination/last"
        , "elimination/fold"
        ]
      )
    , ( "Transformation and Filtering (Shorter is Faster)"
      , [
          "filtering/filter-all-out"
        , "filtering/dropWhile-true"
        , "filtering/take-all"
        , "filtering/takeWhile-true"
        , "transformation/map"
        , "filtering/filter-all-in"
        , "filtering/filter-even"
        , "transformation/scan"
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
        , "transformation/concat"
        ]
      )
    -}
    , ( "Composed (4x) operations (Shorter is Faster)"
      , [ "compose/all-out-filters"
        , "compose/all-in-filters"
        , "compose/mapM"
        , "compose/map-with-all-in-filter"
        ]
      )
    ]

-------------------------------------------------------------------------------

-- "values" is [(packageName, [per benchmark result])]
genGroupGraph :: String -> [String] -> [(String, [Maybe Double])] -> IO ()
genGroupGraph chartTitle benchNames values =
    toFile def (outputDir
                ++ "/"
                -- links in README.rst eat up the space so we match the same
                ++ (filter (not . isSpace) (takeWhile (/= '(') chartTitle))
                ++ ".svg") $ do
        layout_title .= chartTitle
        layout_title_style . font_size .= 25
        layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
        layout_x_axis . laxis_style . axis_label_style . font_size .= 12
        layout_y_axis . laxis_style . axis_label_style . font_size .= 14

        layout <- get
        case _layout_legend layout of
            Nothing -> return ()
            Just style@LegendStyle{..} -> do
                let s = style { _legend_plot_size = 22
                              -- , _legend_margin = 40
                              , _legend_position = LegendBelow
                              , _legend_label_style = _legend_label_style
                                    { _font_size = 14 }
                              }
                put $ layout { _layout_legend = Just s }

        -- layout_y_axis . laxis_override .= axisGridAtTicks
        let modifyLabels ad = ad {
                _axis_labels = map (map (second (++ " ms"))) (_axis_labels ad)
            }
        layout_y_axis . laxis_override .= modifyLabels

        {-
        -- to make comparative charts on a fixed scale
        layout_y_axis . laxis_override .= \_ ->
             let indexes = [0,25..125]
            --let indexes = [0,50..350]
            --let indexes = [0,100..700]
            in makeAxis (map ((++ " ms") . show . floor)) (indexes, [], [])
        -}

        -- XXX We are mapping a missing value to 0, can we label it missing
        -- instead?
        let modifyVal x = map ((*1000) . fromMaybe 0) (snd x)
        plot $ fmap plotBars $ bars benchNames (addIndexes (map modifyVal values))

-- [[Double]] each list is multiple results for each benchmark
transposeLists :: [[a]] -> Maybe [[Maybe a]]
transposeLists xs =
    -- If each benchmark does not have the same number of results then reject
    -- all because the results may not correspond with each other when zipped.
    case nub $ map length xs of
        [0] -> Nothing
        [n] ->
            let ys = map (convertToMaybe n) xs
            in Just $ transpose ys
        [0,n] ->
            -- some packages may have missing benchmarks
            -- fill the empty results with Nothing
            let ys = map (convertToMaybe n) xs
            in Just $ transpose ys
        _ -> Nothing
    where
        convertToMaybe n zs = case zs of
            [] -> replicate n Nothing
            x  -> map Just x

-- Given a package name (e.g. streaming), and benchmark prefixes (e.g.
-- [elimination/null, elimination/toList]) get the corresponding results e.g.
-- [8.1 ms, 5.4 ms]. The corresponding result file entries will have
-- elimination/null/streaming etc. as the names of the entries.
--
-- We return a list of lists as the same benchmark may appear more than once if
-- we ran benchmarks for the same package multiple times. This is helpful in
-- comparing the benchmarks for the same package after fixing something.
getResultsForPackage :: CSV -> String -> [String] -> Maybe [[Maybe Double]]
getResultsForPackage csvData pkgname bmPrefixes =
    let bmnames = map (++ "/" ++ pkgname) bmPrefixes
    in transposeLists $ map getBenchmarkMeans bmnames

    where

    -- field at index 1 is the mean
    getBenchmarkMeans :: String -> [Double]
    getBenchmarkMeans bmname =
        map read $ map (!! 1) $ filter ((== bmname) .  head) csvData

genOneGraph :: CSV -> [(String, String)] -> (String, [String]) -> IO ()
genOneGraph csvData pkginfo (bmGroupTitle, prefixes) =
    genGroupGraph bmGroupTitle bmTitles bmResults

    where

    bmTitles = map (last . splitOn "/" ) prefixes

    pkgName = fst
    pkgVersion = snd
    pkgNameWithVersion pkgInfo = pkgName pkgInfo ++ "-" ++ pkgVersion pkgInfo
    pkgGetResults pkgInfo =
        let res = getResultsForPackage csvData (pkgName pkgInfo) prefixes
        in case res of
            Nothing -> Nothing
            Just xs ->
                case length xs of
                    0 -> Nothing
                    1 -> Just $ map (pkgNameWithVersion pkgInfo,) xs
                    _ -> Just $ zipWith (withIndexes pkgInfo) [(1::Int)..] xs
    withIndexes pkgInfo x y =
        (pkgNameWithVersion pkgInfo ++ "(" ++ show x ++ ")", y)

    -- this produces results for all packages for all prefixes
    -- [(packagenamewithversion, [Maybe Double])]
    bmResults = concat $ catMaybes $ map pkgGetResults pkginfo

genGraphs :: CSV -> [(String, String)] -> IO ()
genGraphs csvData pkginfo = mapM_ (genOneGraph csvData pkginfo) bmGroups

-- XXX display GHC version as well
-- XXX display the OS/arch
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
    let pkginfo' = map (\x -> (x, fromMaybe "" $ lookup x pkginfo)) packages

    csvData <- parseCSVFromFile (head args)
    case csvData of
        Left e -> error $ show e
        Right dat -> genGraphs dat pkginfo'
    return ()
