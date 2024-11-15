## 0.4.0

* Add different streaming libraries under individual flags
* Use the external bench-report package and remove the shell scripts
* User streamly-core package (streamly-0.9.0 changes)

## 0.3.0

* Simplify the README, use text tables instead of graphs.
* Upgrade to latest release of all streaming packages
* Build bench-report utility independently to avoid dependency issues
* Add benchmarks for streamly pure lists, streamly arrays, bytestring,
  text, dlist, sequence
* Add benchmarks to measure composition of the filtering and
  transformation operations multiple times
* Add benchmarks to measure composition of various combinations of different
  operations multiple times.
* Add benchmarks that iterate the same operation multiple times
* Use the `bench-show` package for better reporting of diffs. Supports
  comparison in multiples or percentages of other packages.

## 0.2.0

* Added benchmarks for pure lists
* Added benchmarks for pure `vector`
* Added benchmarks for `vector` monadic streaming library
* Added `drinkery` streaming library
* The code is modular now, package specific ops for each benchmarked package
  are contained in a separate own module. It is much easier to add a new
  package now.
* The benchmarking code now works for `IO` as well as `Identity` monad.
* Used the same stream generation method for all libraries for a fair
  comparison.
* Use a monadic API (`unfoldrM`) for generating the stream.
* conduit-1.3.0 has a performance issue with `mapM_`. Avoided using `mapM_` and
  used `sinkNull` instead. See https://github.com/snoyberg/conduit/issues/363.
  This workaround improves the performance of all conduit benchmarks that drain
  the stream.
* pipes also had an issue similar to that of conduit. The code was using
  `mapM_` which was very inefficient, used `discard` instead and got a
  significant boost in numbers.

## 0.1.0

* Initial release
