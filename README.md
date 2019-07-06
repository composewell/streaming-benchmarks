# Streaming Benchmarks

[![Hackage](https://img.shields.io/hackage/v/streaming-benchmarks.svg?style=flat)](https://hackage.haskell.org/package/streaming-benchmarks)
[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Build Status](https://travis-ci.org/composewell/streaming-benchmarks.svg?branch=master)](https://travis-ci.org/composewell/streaming-benchmarks)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/8d1kgrrw9mmxv5xt?svg=true)](https://ci.appveyor.com/project/harendra-kumar/streaming-benchmarks)

Compare the performance of various implementations of pure streams, monadic
streams and arrays in Haskell. Due care has been taken to keep the comparisons
fair.  Please send an email or a PR if the benchmarking code has a
problem or is unfair to some library in any way.

DISCLAIMER: This package is a result of benchmarking effort done during the
development of Streamly by the authors of Streamly.

## Benchmark Setup

A stream of one million consecutive integers is generated and various stream
operations are applied on the stream.  The total time taken, and the maximum
resident set size (rss) is measured and plotted for each library.

Libraries are built with GHC-8.6.5. All benchmarks were run on an Apple MacBook
Pro computer with a 4-core 2.2 GHz Intel Core i7 processor and 16GB RAM.

## Benchmarks

* `drain`                  | Just discards all the elements in the stream
* `drop-all`               | drops all element using the ``drop`` operation
* `last`                   | extract the last element of the stream
* `fold`                   | sum all the numbers in the stream
* `map`                    | increments each number in the stream by 1
* `take-all`               | Use ``take`` to retain all the elements in the stream
* `filter-even`            | Use ``filter`` to keep even numbers and discard odd numbers in the stream.
* `scan`                   | scans the stream using ``+`` operation
* `mapM`                   | transform the stream using a monadic action
* `zip`                    | combines corresponding elements of the two streams together

In addition to basic operations like in the table above we also have benchmarks
with repeated application of the same operation e.g. `take-all x 4` means the
take-all operations performed 4 times repeatedly. Similalrly we have composite
operations e.g. `take-map` is a `take` operation followed by a `map` operation.
Then we have repeated composite operations e.g. `take-map x 4` is `take-map`
applied 4 times.

## Pure Streams

Streamly stream type `SerialT Identity a` can be used as a pure stream
replacing `[a]`. Using `OverloadedLists` GHC extension streamly can be used as
an almost drop-in replacement for lists. See `Streamly.List` module for more
details.

The following figures show the ratio of time and memory consumed by [Int] vs
`SerialT Identity Int` for exactly the same operation. `5x` on the y axis means
lists take 5 times more resources compared to streamly. Whereas a `-5x` would
mean that lists takes 5 times less resources compared to streamly. We only show
those operations which are at least 10% better or worse in one library compared
to the other. The operations are shown in a sorted order, from list's worst
performing ones on the left to its best ones on the right.

Time Graphs:

![Streamly vs Lists (time) comparison](charts-0/by'list'intermsof'pure-streamly'-median-time.svg)

Memory Graphs:

![Streamly vs Lists (memory) comparison](charts-0/by'list'intermsof'pure-streamly'-median-maxrss.svg)

See full details on timing and memory utilization of all operations benchmarked [here](charts-0/streamly-vs-list.txt)

Streamly uses stream fusion whereas lists use foldr/build fusion. The reason
why streamly performs much better than lists for mixed operations could be
because of better fusion. It is also possible that something needs to be fixed
in lists. We hope that these results will lead to some investigation and fixing
of the libraries we measured.

## Monadic Streams

* Streamly, streaming, pipes, conduit, machines

``unfoldrM`` is used to generate the stream for two reasons, (1) it is
monadic, (2) it reduces the generation overhead so that the actual streaming
operation cost is amplified. If we use generation from a list there is a
significant overhead in the generation itself because of the intermediate
list structure.

## Arrays

* Streamly.Array, vector (Storable), text, bytestring

## Streams of Arrays

* streamly (Streamly.Array), bytestring (Data.ByteString.Lazy)


## How to Run

To quickly compare packages:

```
# Show help
$ ./bench.sh --help

# Compare a given list of packages. Use `--help` for available package names.
$ ./bench.sh --diff fraction --benchmarks "streamly,streaming"
$ ./bench.sh --diff fraction --benchmarks "streamly,conduit,pipes"

# Show percent diff
$ ./bench.sh --diff percent --benchmarks "streamly,streaming"

# Show absolute values instead of diff
$ ./bench.sh --benchmarks "streamly,streaming"

# Generate graphs (.svg) instead of textual comparison
$ ./bench.sh --diff fraction --benchmarks "streamly,conduit,pipes" --graphs
```

## Adding New Libraries

It is trivial to add a new package. This is how 
[a benchmark file](https://github.com/composewell/streaming-benchmarks/blob/master/Benchmarks/Streamly.hs)
for a streaming package looks like. Pull requests are welcome, we will be happy
to help, [just join the gitter chat](https://gitter.im/composewell/streamly)
and ask!

## Benchmarking Notes

* Unoptimized `NFData` instances for some libraries may cause false poor
  performance results. We use our own custom evaluation routines to avoid that.
* Memory once requested from the OS is never released back to the OS by `GHC`.
  This may lead to false `maxrss` reporting when there are multiple benchmarks in
  the same benchmark recipe file. We run each benchmark in an isolated
  process to avoid that. The `bench.sh` script takes care of this, running the
  benchmark executable directly may not give correct results if all benchmarks
  are run together.
* Running benchmarks in isolation also ensures avoiding any other kind of
  interference (e.g. unwanted sharing) among benchmarks. Though this may not
  be able to avoid any compile time interference.
* We have tried to optimize (`INLINE` etc.) each library's code as much as we
  could, library authors are encouraged to take a look at if their library is
  being used in a fully optimized manner and report if there are any issues.
* The basic `drain` benchmark generates a stream and drains it without doing
  any operation on the elements. This drain operation is part of all
  benchmarks, therefore all operations would include the cost of drain. If
  draining is slow for a library then all other operations also show up as
  slow.
