Streaming Benchmarks
--------------------

Comprehensive, carefully crafted benchmarks for streaming operations and their
comparisons across notable Haskell streaming libraries including `streaming`,
`machines`, `pipes`, `conduit` and `streamly`, the new library that we
published.

How to Run
----------

::

  ./run.sh

Diagnostics
~~~~~~~~~~~

If ``run.sh`` fails with an error like this:

```
callProcess: runInteractiveProcess: exec: does not exist (No such file or directory)
```

it means that it cannot find the path to benchmarking executable. You will have
to find it manually and edit it in ``run.sh``. See the comments in ``run.sh``.

Results
-------

Important Points about Benchmarking Methodology
-----------------------------------------------

``IO Monad:`` We run the benchmarks in the IO monad so that they are close to
real life usage. Note that most existing streaming benchmarks use pure code or
Identity monad which may produce entirely different results.

``Benchmarking Tool:`` We use the ``gauge`` package instead of criterion.  We
spent a lot of time figuring out why benchmarking was not producing accurate
results. Criterion had several bugs due to which results were not reliable. We
fixed those bugs in ``gauge``. For example due to GC or CAF evaluation
interaction across benchmarks, the results of benchmarks running later in the
sequence were sometimes totally off the mark. We fixed that by running each
benchmark in a separate process in guage. Another bug caused criterion to
report wrong mean.

``Iterations:`` We pass a million elements through the streaming pipelines. We
do not rely on the benchmarking tool for this, it is explicitly done by the
benchmarking code and the benchmarking tool is asked to perform just one
iteration. We added fine grained control in `gauge` to be able to do this.

``Effects of Optimizations:`` In some cases fusion or other optimizations can
just optimize out everything and produce ridiculously low results. To avoid
that we generate random numbers in the IO monad and pass those through the
pipeline rather than using some constant or predictable source.

``GHC Optimization Flags:`` To make sure we are comparing fairly we make sure
that we compile the benchmarking code, the library code as well as all
dependencies using exactly the same GHC flags. GHC inlining and specialization
optimizations can make the code unpredictable if mixed flags are used.

``Benchmark Categories:`` We have two categories of benchmarks, one to measure
the performance of individual operations in isolation and the other to measure
the performance when multiple similar or different operations are composed
together in a pipeline.

Benchmarks
----------

Individual Operations
~~~~~~~~~~~~~~~~~~~~~

Elimination
^^^^^^^^^^^

* `null:` This is the simplest of all benchmarks. It compares creating a
  source and immediately composing it with a sink without any processing in
  between.

* `toList:` This is just like `null` except that instead of sending the
  stream to a sink it collects it in a list.

* `fold`, `scan`, `last`, `concat` are other elimination operations that we
  benchmark.

Transformation
^^^^^^^^^^^^^^

The operations in this category are the standard `map` and `mapM`.

Filtering
^^^^^^^^^

This category includes the standard `filter`, `take`, `takeWhile`, `drop` and
`dropWhile` operations.

Zipping
^^^^^^^

Zipping two streams together.

Composing Multiple Pipeline Stages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This category measures the performance when multiple operations are composed in
a pipeline.

The `mapM` benchmark introduces four stages of `mapM` between the source and
the sink.

`passing-filters` composes four stages of a `filter` operation that passes all
the items through.  Note that passing or blocking nature of the filter may
impact the results.

`blocking-filters` composes four stages of a `filter` operation that `blocks`
all the items i.e. does not let anything pass through.

The `map-filter` benchmark introduces four identical stages between the source
and the sink where each stage performs a simple map operation followed by a
filter that passes all the items through.


Studying the Scaling of Composition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This category of benchmarks studies the effect of adding more stages in a
composition pipeline. For each library it displays the results when 1, 2, 3 or
4 pipeline stages are used.

Benchmarking Errors
-------------------

Benchmarking is a tricky business. Though the benchmarks have been carefully
designed there may still be issues with the way benchmarking is being done or
the way they have been coded. If you find that something is being measured
unfairly or incorrectly please bring it to our notice by raising an issue or
sending an email.
