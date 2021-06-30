# Benchmarking Correctly

## Reduce noise overhead

A common mistake in writing benchmarks is to mask signal with noise.
For example, if we write the benchmarks such as to convert the stream
to a list then the time consumed by list creation itself will mask the
stream processing time.  The total time would be:

```
  total = n * list cons overhead + n * stream processing overhead
```

If the list cons overhead is too high it will just mask the stream
processing time which is what we are really interested in.  We need to
ensure that any such noise factor is either small enough to neglect or
we measure and deduct it from the results.

## Describe signal and noise

Ideally in case of micro benchmarks, with each benchmark we need to
explain what exactly is being measured or what is the benchmarking code
doing. For example, in the previous example we can mention that the
benchmark measures the list creation time plus the stream operation time;
the list creation time dominates.

Each benchmark should have a description of what it is measuring.

## Caveats for fair comparison

To avoid the domination of list creation time we should choose a
stream exit operation that takes the minimum amount of time. For
example, we could use a sum fold on the output of the stream instead of
converting it into a list. We assume that the fold time would be much
lower. However, this means that we are measuring the fold time + the
stream operation time. If the fold time for a particular library is
high then we are really measuring the fold performance for that rather
than measuring the performance of that particular operation.

The ideal would be to break down the components involved in a
benchmark and show each component separately. However, in the
real world it is a combination of operations that we are going
to use. That's why micro benchmarks are only useful to optimize
individual operations the overall performance may be dominated by the
most frequent operations and the weakest link in the whole chain.

## Measuring maxrss

Memory once requested from the OS is never released back to the OS by
`GHC` (this has changed since GHC 9.2).  This may lead to false `maxrss`
reporting when there are multiple benchmarks in the same benchmark
recipe file. We run each benchmark in an isolated process to avoid
that. The `bench.sh` script takes care of this, running the benchmark
executable directly may not give correct results if all benchmarks are
run together.

## Avoiding interference across benchmarks

Running benchmarks in isolation also ensures avoiding any other kind of
interference (e.g. unwanted sharing) among benchmarks. Though this may
not be able to avoid any compile time interference, e.g. sharing of data
across benchmarks may lead GHC to generate suboptimal code.

## Fragile inlining

We have tried to optimize (`INLINE` etc.) each library's code as much as we
could, library authors are encouraged to take a look at if their library is
being used in a fully optimized manner and report if there are any issues.
