## Unreleased

* Added benchmarks for `vector` monadic streaming library
* Added `drinkery` streaming library
* The code is modular now, package specific ops for each benchmarked package
  are contained in a separate own module. It is much easier to add a new
  package now.
* The benchmarking code now works for `IO` as well as `Identity` monad.
* Used the same stream generation method for all libraries for a fair
  comparison.
* conduit-1.3.0 has a performance issue with `mapM_`. Avoided using `mapM_` and
  used `sinkNull` instead. See https://github.com/snoyberg/conduit/issues/363.
  This workaround improves the performance of all conduit benchmarks that drain
  the stream.
* pipes also had an issue similar to that of conduit. The code was using
  `mapM_` which was very inefficient, used `discard` instead and got a
  significant boost in numbers.

## 0.1.0

* Initial release
