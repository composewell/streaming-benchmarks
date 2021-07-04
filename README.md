# Streaming Benchmarks

[![Hackage](https://img.shields.io/hackage/v/streaming-benchmarks.svg?style=flat)](https://hackage.haskell.org/package/streaming-benchmarks)
[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Build Status](https://travis-ci.org/composewell/streaming-benchmarks.svg?branch=master)](https://travis-ci.org/composewell/streaming-benchmarks)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/8d1kgrrw9mmxv5xt?svg=true)](https://ci.appveyor.com/project/harendra-kumar/streaming-benchmarks)

This package provides micro-benchmarks to measure and compare the
performance of various streaming implementations in Haskell.

We have taken due to care to make sure that we are
benchmarking correctly and fairly. See [the notes on correct
benchmarking](docs/benchmarking-notes.md).

DISCLAIMER: This package is a result of benchmarking effort done during the
development of [streamly](https://github.com/composewell/streamly) by the
authors of [streamly](https://github.com/composewell/streamly).

## Benchmarks

The benchmark names are obvious, some of them are described below.  Single
operation benchmarks:

| Name           | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| `drain`        | Just discards all the elements in the stream                |
| `drop-all`     | drops all element using the ``drop`` operation              |
| `last`         | extract the last element of the stream                      |
| `fold`         | sum all the numbers in the stream                           |
| `map`          | increments each number in the stream by 1                   |
| `take-all`     | Use ``take`` to retain all the elements in the stream       |
| `filter-even`  | Keep even numbers, discard odd                              |
| `scan`         | scan the stream using ``+`` operation                       |
| `mapM`         | transform the stream using a monadic action                 |
| `zip`          | combines corresponding elements of the two streams together |

Composite operation benchmarks:

| Name           | Description                                                 |
| -------------- | ----------------------------------------------------------- |
| `map x 4`      | perform `map` operation 4 times |
| `take-map`     | `take` followed by a `map` |

For more details on how each benchmark is implemented see
[this benchmark file](https://github.com/composewell/streaming-benchmarks/blob/master/Benchmarks/Streamly.hs).

Each benchmark is run in a separate process to avoid any effects of GC
interference and sharing across benchmarks.

## Benchmark Results

Below we present some results comparing
[streamly](https://github.com/composewell/streamly) with other streaming
implementations.
Due care has been taken to keep the comparisons fair.  We have optimized
each library's code to the best of our knowledge, please point out if
you find any measurement issues.

### Reproducing benchmark results

Commands to reproduce the benchmark results are provided in each section
below. But before you run those commands you need to build the reporting
tool once using the following command. Note that this command works with
only ghc-8.8.4 or lower. However, after building this tool you can run
the benchmarks with any GHC version.

```
$ bin/bench.sh --with-compiler ghc-8.8.4 --no-measure
```

Nix users can use `--use-nix` option. It uses an older version of
nixpkgs that contains the required dependencies. For example:

```
$ bin/bench.sh --use-nix --quick
```

### Streamly vs Haskell Lists

Streamly, when used with `Identity` monad, is almost the same as Haskell lists
(in the `base` package).
[See this](https://github.com/composewell/streamly/blob/master/docs/streamly-vs-lists.md)
for more details.

The following table compares the timing of several operations for
[streamly](https://github.com/composewell/streamly)
with lists using a one million element stream.  For brevity only
those operations where the performance of the two packages differ by
more than 10% are shown in the table below. The last column shows how
many times slower list is compared to streamly.

| Benchmark           | streamly(μs) |  list(μs)  | list/streamly |
| ------------------- | ------------ | ---------- | ------------- |
| drop-map x 4        |    375.09    |  76925.32  | 205.08        |
| filter-drop x 4     |    382.03    |  54848.54  | 143.57        |
| drop-scan x 4       |    795.81    |  76716.79  |  96.40        |
| filter-scan x 4     |    795.60    |  44559.15  |  56.01        |
| scan-map x 4        |   1192.19    |  48838.22  |  40.97        |
| take-map x 4        |   1500.99    |  60126.58  |  40.06        |
| filter-take x 4     |   1502.01    |  48766.87  |  32.47        |
| take-drop x 4       |   1499.62    |  41720.03  |  27.82        |
| take-scan x 4       |   1874.94    |  51283.30  |  27.35        |
| drop-one x 4        |    375.33    |   8993.87  |  23.96        |
| dropWhile-false x 4 |    374.61    |   8957.79  |  23.91        |
| dropWhile-false     |    374.83    |   8670.05  |  23.13        |
| drop-one            |    390.77    |   8681.85  |  22.22        |
| dropWhile-true      |    571.60    |  12237.48  |  21.41        |
| drop-all            |    562.94    |   8262.38  |  14.68        |
| take-all            |    624.83    |    564.34  |  1/1.11       |
| scan x 4            |    795.83    |    385.85  |  1/2.06       |
| appendR[10000]      |    360.75    |    126.95  |  1/2.84       |
| concatMap           |   34957.71   |   1124.85  |  1/31.08      |

* streamly-0.8.0, base-4.14.1.0, ghc-8.10.4, Linux

To reproduce these results use the following commands:

```
$ bin/bench.sh --benchmarks "StreamlyPure List" --compare --diff-style absolute --diff-cutoff-percent 10 --quick
$ bin/bench.sh --benchmarks "StreamlyPure List" --compare --diff-style multiples --diff-cutoff-percent 10 --quick
```

### Streamly vs Streaming

The following table compares the timing of several operations
for [streamly](https://github.com/composewell/streamly) with
[streaming](https://hackage.haskell.org/package/streaming) using a
million element stream.

| Benchmark           | streamly(μs) | streaming(μs) | streaming/streamly |
| ------------------- | ------------ | ------------- | ------------------ |
| appendR[10000]      |       326.56 |    1301176.69 |            3984.54 |
| mapM x 4            |       374.42 |     223591.08 |             597.17 |
| filter-map x 4      |       381.07 |     194903.88 |             511.47 |
| filter-scan x 4     |       795.66 |     233527.90 |             293.50 |
| filter-all-in x 4   |       375.40 |     102629.64 |             273.38 |
| filter-drop x 4     |       387.15 |      99096.98 |             255.96 |
| map x 4             |       386.49 |      94944.87 |             245.66 |
| drop-map x 4        |       375.62 |      89669.37 |             238.73 |
| scan x 4            |       797.00 |     166332.40 |             208.70 |
| scan-map x 4        |      1194.30 |     238804.48 |             199.95 |
| filter-even x 4     |       396.37 |      77865.47 |             196.45 |
| drop-scan x 4       |       796.98 |     156063.52 |             195.82 |
| takeWhile-true x 4  |       562.49 |      90183.53 |             160.33 |
| scan                |       375.24 |      47520.57 |             126.64 |
| filter-take x 4     |      1498.55 |     189635.34 |             126.55 |
| mapM                |       388.10 |      46689.61 |             120.30 |
| take-map x 4        |      1500.71 |     178954.50 |             119.25 |
| zip                 |       656.65 |      66689.73 |             101.56 |
| take-scan x 4       |      2380.35 |     241675.75 |             101.53 |
| filter-all-in       |       375.97 |      33590.14 |              89.34 |
| map                 |       375.02 |      33081.13 |              88.21 |
| filter-even         |       393.26 |      30458.46 |              77.45 |
| filter-all-out      |       382.87 |      26826.21 |              70.07 |
| take-all x 4        |      1499.71 |     101332.53 |              67.57 |
| take-drop x 4       |      1498.53 |      98281.99 |              65.59 |
| takeWhile-true      |       562.62 |      31863.25 |              56.63 |
| foldl'              |       388.22 |      18503.15 |              47.66 |
| drop-all            |       562.08 |      25200.32 |              44.83 |
| take-all            |       768.65 |      33247.97 |              43.26 |
| dropWhile-true      |       564.87 |      24431.50 |              43.25 |
| last                |       385.53 |      15240.85 |              39.53 |
| dropWhile-false     |       374.83 |      14566.70 |              38.86 |
| drop-one            |       374.80 |      14565.01 |              38.86 |
| drop-one x 4        |       375.88 |      14448.67 |              38.44 |
| dropWhile-false x 4 |       390.12 |      14619.42 |              37.47 |
| drain               |       375.06 |      13702.29 |              36.53 |
| toList              |    117708.83 |     201444.81 |               1.71 |

* streamly-0.8.0, streaming-0.2.3.0, ghc-8.10.4, Linux

To reproduce these results use the following commands:

```
$ bin/bench.sh --benchmarks "Streamly Streaming" --compare --diff-style absolute --diff-cutoff-percent 10 --quick
$ bin/bench.sh --benchmarks "Streamly Streaming" --compare --diff-style multiples --diff-cutoff-percent 10 --quick
```

### Streamly vs Pipes

The following table compares the timing of several operations
for [streamly](https://github.com/composewell/streamly) with
[pipes](https://hackage.haskell.org/package/pipes) using a
million element stream.

| Benchmark           |  streamly(μs)  |  pipes(μs)  | pipes/streamly |
| ------------------- |  ------------  |  ---------  | -------------- |
| appendR[10000]      |        327.90  |  901135.92  |        2748.21 |
| mapM x 4            |        375.20  |  407184.39  |        1085.23 |
| filter-map x 4      |        381.52  |  366759.70  |         961.31 |
| drop-map x 4        |        375.48  |  281296.82  |         749.16 |
| filter-all-in x 4   |        375.60  |  222331.68  |         591.93 |
| filter-drop x 4     |        387.44  |  222830.71  |         575.14 |
| drop-scan x 4       |        797.23  |  336737.89  |         422.39 |
| filter-even x 4     |        389.87  |  152688.91  |         391.64 |
| filter-scan x 4     |        797.38  |  309733.91  |         388.44 |
| drop-one x 4        |        375.48  |  139851.13  |         372.46 |
| map x 4             |        386.56  |  136289.32  |         352.57 |
| dropWhile-false x 4 |        390.72  |  137395.44  |         351.65 |
| scan-map x 4        |       1194.38  |  381286.88  |         319.23 |
| takeWhile-true x 4  |        562.86  |  165143.23  |         293.40 |
| scan x 4            |        796.68  |  222986.17  |         279.90 |
| mapM                |        388.19  |   95576.97  |         246.21 |
| filter-all-in       |        375.21  |   71297.42  |         190.02 |
| take-map x 4        |       1502.76  |  275887.24  |         183.59 |
| scan                |        374.81  |   65549.13  |         174.89 |
| take-drop x 4       |       1503.43  |  256448.45  |         170.58 |
| filter-even         |        390.29  |   66183.72  |         169.57 |
| filter-all-out      |        376.99  |   59074.54  |         156.70 |
| drop-one            |        375.19  |   58395.24  |         155.64 |
| dropWhile-false     |        375.35  |   58223.03  |         155.12 |
| map                 |        375.05  |   57736.43  |         153.94 |
| filter-take x 4     |       1503.00  |  227925.71  |         151.65 |
| take-scan x 4       |       2455.91  |  354284.33  |         144.26 |
| zip                 |        657.07  |   86011.93  |         130.90 |
| takeWhile-true      |        564.14  |   61390.21  |         108.82 |
| take-all x 4        |       1502.32  |  139730.70  |          93.01 |
| dropWhile-true      |        564.03  |   49227.19  |          87.28 |
| drop-all            |        562.05  |   46505.37  |          82.74 |
| take-all            |        824.09  |   60511.34  |          73.43 |
| drain               |        375.29  |   26390.59  |          70.32 |
| foldl'              |        397.34  |   19064.05  |          47.98 |
| last                |        387.11  |   17364.44  |          44.86 |
| toList              |     117257.09  |  207405.94  |           1.77 |

* streamly-0.8.0, pipes-4.3.16, ghc-8.10.4, Linux

To reproduce these results use the following commands:

```
$ bin/bench.sh --benchmarks "Streamly Pipes" --compare --diff-style absolute --diff-cutoff-percent 10 --quick
$ bin/bench.sh --benchmarks "Streamly Pipes" --compare --diff-style multiples --diff-cutoff-percent 10 --quick
```

### Streamly vs Conduit

The following table compares the timing of several operations
for [streamly](https://github.com/composewell/streamly) with
[conduit](https://hackage.haskell.org/package/conduit) using a
million element stream.

| Benchmark           | streamly(μs) | conduit(μs)  | conduit/streamly |
| ------------------- | ------------ | -----------  | ---------------- |
| mapM x 4            |       375.46 |   297002.31  |           791.04 |
| filter-map x 4      |       380.79 |   267543.81  |           702.60 |
| drop-map x 4        |       375.66 |   232307.84  |           618.39 |
| filter-drop x 4     |       386.05 |   235029.15  |           608.81 |
| filter-scan x 4     |       796.56 |   306556.67  |           384.85 |
| drop-scan x 4       |       797.19 |   300789.06  |           377.31 |
| zip                 |       657.29 |   210069.05  |           319.60 |
| filter-all-in x 4   |       375.24 |   118506.68  |           315.82 |
| scan-map x 4        |      1194.67 |   360671.18  |           301.90 |
| map x 4             |       387.00 |   113497.14  |           293.27 |
| drop-one x 4        |       375.49 |   101842.95  |           271.23 |
| dropWhile-false x 4 |       389.44 |   102051.22  |           262.04 |
| scan x 4            |       796.72 |   190479.35  |           239.08 |
| takeWhile-true x 4  |       564.58 |   114459.57  |           202.73 |
| filter-even x 4     |       391.76 |    72369.30  |           184.73 |
| filter-take x 4     |      1502.04 |   267921.27  |           178.37 |
| take-map x 4        |      1502.88 |   238875.95  |           158.95 |
| take-drop x 4       |      1500.34 |   232606.19  |           155.04 |
| take-scan x 4       |      2443.83 |   309738.86  |           126.74 |
| mapM                |       389.15 |    41897.48  |           107.66 |
| scan                |       375.40 |    38137.85  |           101.59 |
| take-all x 4        |      1502.32 |   110682.74  |            73.67 |
| filter-all-in       |       375.31 |    26024.21  |            69.34 |
| dropWhile-false     |       375.10 |    25307.13  |            67.47 |
| map                 |       375.18 |    23088.09  |            61.54 |
| drop-one            |       375.43 |    22020.65  |            58.65 |
| filter-even         |       392.28 |    21504.28  |            54.82 |
| takeWhile-true      |       562.79 |    29012.68  |            51.55 |
| filter-all-out      |       378.76 |    15736.05  |            41.55 |
| drop-all            |       562.89 |    19916.48  |            35.38 |
| foldl'              |       388.88 |    12499.03  |            32.14 |
| dropWhile-true      |       564.43 |    17983.35  |            31.86 |
| take-all            |       784.67 |    24425.36  |            31.13 |
| last                |       385.75 |    10974.84  |            28.45 |
| drain               |       375.18 |     4272.15  |            11.39 |
| appendR[10000]      |       326.93 |     1207.88  |             3.69 |
| toList              |    116441.26 |   199138.09  |             1.71 |

* streamly-0.8.0, conduit-1.3.4.1, ghc-8.10.4, Linux

To reproduce these results use the following commands:

```
$ bin/bench.sh --benchmarks "Streamly Conduit" --compare --diff-style absolute --diff-cutoff-percent 10 --quick
$ bin/bench.sh --benchmarks "Streamly Conduit" --compare --diff-style multiples --diff-cutoff-percent 10 --quick
```

## Stack and heap utilization

To report heap utilization by individual benchmarks you can include
`maxrss` in the `--fields` option.

To know about stack and heap utilization by the libraries you can also take a
look at the RTS heap and stack limits used to run the benchmarks of various
libraries in [bench-config.sh](bin/bench-config.sh).

## Comparing other libraries

This package supports many streaming libraries. Use the following command to
see all available benchmarks:

```
$ ./bench.sh --help
```

You can then select the libraries you want to compare:

```
$ ./bench.sh --benchmarks "streaming,pipes" --measure
```

## Adding New Libraries

It is trivial to add a new package. This is how
[a benchmark file](https://github.com/composewell/streaming-benchmarks/blob/master/Benchmarks/Streamly.hs)
for a streaming package looks like. Pull requests are welcome, we will be happy
to help, [just join the gitter chat](https://gitter.im/composewell/streamly)
and ask!
