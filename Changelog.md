## Unreleased

* The code is modular now, package specific ops for each benchmarked package
  are contained in a separate own module. It is much easier to add a new
  package now.

* conduit-1.3.0 has a performance issue with `mapM_`. Avoided using `mapM_` and
  used `sinkNull` instead. See https://github.com/snoyberg/conduit/issues/363.
  This workaround improves the performance of all conduit benchmarks that drain
  the stream.

## 0.1.0

* Initial release
