import Data.List.Split (splitOn)
import Data.Maybe (maybe, catMaybes, fromJust)
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
packages = ["streamly", "streaming", "conduit", "pipes", "machines"]

-- pairs of benchmark group titles and corresponding list of benchmark
-- prefixes i.e. without the package name at the end.
bmGroups :: [(String, [String])]
bmGroups =
    [
      ( "Elimination"
      , [ "elimination/null"
        , "elimination/toList"
        , "elimination/fold"
        , "elimination/scan"
        , "elimination/last"
        , "elimination/concat"
        ]
      )
    , ( "Transformation"
      , [ "transformation/map"
        , "transformation/mapM"
        ]
      )
    , ( "Filtering"
      , [ "filtering/filter"
        , "filtering/take"
        , "filtering/takeWhile"
        , "filtering/drop"
        ]
      )
    , ( "Zipping"
      , [ "zip"
        ]
      )
    , ( "Composition"
      , [ "compose/mapM"
        , "compose/passing-filters"
        , "compose/blocking-filters"
        , "compose/map-filter"
        ]
      )
    ]

-------------------------------------------------------------------------------

-- "values" has results for each package for each title in bmTitles
genGroupGraph :: String -> [String] -> [(String, [Maybe Double])] -> IO ()
genGroupGraph bmGroupName bmTitles values =
    toFile def (outputDir ++ "/" ++ bmGroupName ++ ".svg") $ do
        layout_title .= bmGroupName
        layout_title_style . font_size .= 25
        layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
        -- XXX We are mapping a missing value to 0, can we label it missing
        -- instead?
        let getVal x = map (maybe 0 id) (snd x)
        plot $ fmap plotBars $ bars bmTitles (addIndexes (map getVal values))

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
            xs -> Just (read ((last xs) !! 2))

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
