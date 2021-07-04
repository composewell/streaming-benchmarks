#!/usr/bin/env bash

# Customization options
bench_config () {
  BENCH_REPORT_DIR=bench-report
  BENCHMARK_PACKAGE_NAME=streaming-benchmarks
  BENCHMARK_PACKAGE_VERSION=0.3.0

  USE_GAUGE=1
  DEFAULT_FIELDS="allocated cputime"
}

#------------------------------------------------------------------------------
# benchmark groups
#------------------------------------------------------------------------------

bench_targets () {
  INDIVIDUAL_TARGETS="\
     Streamly \
     StreamlyPure \
     List \
     Streaming \
     Machines \
     Pipes \
     Conduit \
     Drinkery \
     VectorStreams \
     ByteStringLazy"
}

#------------------------------------------------------------------------------
# RTS options based on the benchmark executable name and benchmark name
#------------------------------------------------------------------------------

bench_rts_options () {
  local exe_name
  local bench_name

  exe_name="$1"
  bench_name="$2"

  # All benchmarks
  case "$bench_name" in
    *) echo -n "-K36K -M16M" ;;
  esac

  echo -n " "

  case "$exe_name" in
    Conduit) echo -n "-M256M" ;;
    Machines) echo -n "-K16M -M256M" ;;
    Drinkery) echo -n "-K256M -M128M" ;;
    Pipes) echo -n "-K1M -M64M" ;;
    Streaming) echo -n "-K256K" ;;
    VectorStreams) echo -n "-K256K -M256M" ;;
    *) echo -n "" ;;
  esac

  echo -n " "
  # Based on specific benchmark
  # XXX Note: for tasty-bench we replace the "." separator in the
  # benchmark names with "/" for matching with this. It may not work
  # reliably if the benchmark name already contains ".".
  case "$bench_name" in
    */iterated/*) echo -n "-K4M" ;;
    */toList) echo -n "-K16M -M64M" ;;
    *) echo -n "" ;;
  esac
}

#------------------------------------------------------------------------------
# Speed options
#------------------------------------------------------------------------------

bench_speed_options () {
  local exe_name
  local bench_name

  exe_name="$1"
  bench_name="$2"

  case "$exe_name" in
    #Prelude.Concurrent) set_super_quick_mode ;;
    *) echo -n "" ;;
  esac

  # XXX Note: for tasty-bench we replace the "." separator in the
  # benchmark names with "/" for matching with this. It may not work
  # reliably if the benchmark name already contains ".".

  # Use quick options for benchmarks that take too long
  case "$bench_name" in
    #Prelude.Parallel/o-n-heap/mapping/mapM) set_super_quick_mode ;;
    #Prelude.Parallel/o-n-heap/concat-foldable/*) use_quicker_mode ;;
    *) echo -n "" ;;
  esac
}
