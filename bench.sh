#!/bin/bash

print_help () {
  echo "Usage: $0 "
  echo "       [--packages <streamly,vector,pure-vector,list,streaming,pipes,conduit,machines>]"
  echo "       [--graphs]"
  echo "       [--delta]"
  echo "       [--slow]"
  echo "       [--no-measure]"
  echo "       [--append] "
  echo "       [--versions] "
  echo "       -- <gauge options>"
  echo
  echo "--graphs: generate SVG graphs instead of text reports"
  echo "--delta: show diff of subsequent packages from the first package"
  echo "--slow: slower but a bit more precise benchmarking"
  echo "--no-measure: don't measure, generate reports from previous measurements"
  echo "--append: append the new measurement results to previous ones for comparison"
  echo "--versions: add package versions in the report/graphs"
  echo
  echo "Any arguments after a '--' are passed directly to guage"
  exit
}

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

set_packages() {
  if test -z "$PACKAGES"
  then
    PACKAGES=$DEFAULT_PACKAGES
  elif test "$PACKAGES" = "all"
  then
    PACKAGES=$ALL_PACKAGES
  fi
  echo "Using packages [$PACKAGES]"
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to guage as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.
# Use this command to find the exe if this script fails with an error:
# find .stack-work/ -type f -name "benchmarks"

find_bench_prog () {
  local bench_name=$1
  local bench_prog=`$STACK path --dist-dir`/build/$bench_name/$bench_name
  if test -x "$bench_prog"
  then
    echo $bench_prog
  else
    return 1
  fi
}

bench_output_file() {
    echo "charts/results.csv"
}

# --min-duration 0 means exactly one iteration per sample. We use a million
# iterations in the benchmarking code explicitly and do not use the iterations
# done by the benchmarking tool.
#
# Benchmarking tool by default discards the first iteration to remove
# aberrations due to initial evaluations etc. We do not discard it because we
# are anyway doing iterations in the benchmarking code and many of them so that
# any constant factor gets amortized and anyway it is a cost that we pay in
# real life.
#
# We can pass --min-samples value from the command line as second argument
# after the benchmark name in case we want to use more than one sample.

run_bench () {
  local bench_name=benchmarks
  local output_file=$(bench_output_file)
  local bench_prog
  bench_prog=$(find_bench_prog $bench_name) || \
    die "Cannot find benchmark executable for benchmark $bench_name"

  mkdir -p `dirname $output_file`

  echo "Running benchmark $bench_name ..."

  local MATCH_ARGS=""
  for i in $(echo $PACKAGES | tr "," "\n")
  do
     MATCH_ARGS="$MATCH_ARGS -m pattern /$i"
  done

  $bench_prog $SPEED_OPTIONS \
    --csvraw=$output_file \
    -v 2 \
    --measure-with $bench_prog $MATCH_ARGS $GAUGE_ARGS \
      || die "Benchmarking failed"
}

backup_output_file() {
  local output_file=$(bench_output_file)

  if test -e $output_file -a "$APPEND" != 1
  then
      mv -f -v $output_file ${output_file}.prev
  fi
}

run_measurements() {
  backup_output_file
  run_bench
}

run_reports() {
  local packages=$1
  local output_file=$(bench_output_file)
  echo
  echo "Generating reports/charts from ${output_file}..."
  $STACK exec makecharts $output_file $packages $GRAPH $DELTA $VERSIONS
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

DEFAULT_PACKAGES="streamly,vector,streaming,conduit,pipes,machines,drinkery"
ALL_PACKAGES="streamly,vector,streaming,conduit,pipes,machines,drinkery"
DELTA=False

APPEND=0
RAW=0
GRAPH=False
VERSIONS=False
MEASURE=1
SPEED_OPTIONS="--quick --min-samples 10 --time-limit 1 --min-duration 0"

STACK=stack
GAUGE_ARGS=

BUILD_ONCE=0

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    --slow) SPEED_OPTIONS="--min-duration 0"; shift ;;
    --append) APPEND=1; shift ;;
    --packages) shift; PACKAGES=$1; shift ;;
    --delta) DELTA=True; shift ;;
    --raw) RAW=1; shift ;;
    --graphs) GRAPH=True; shift ;;
    --versions) VERSIONS=True; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --) shift; break ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done
GAUGE_ARGS=$*

echo "Using stack command [$STACK]"
set_packages

#-----------------------------------------------------------------------------
# Build stuff
#-----------------------------------------------------------------------------

$STACK build --bench --no-run-benchmarks || die "build failed"

#-----------------------------------------------------------------------------
# Run benchmarks
#-----------------------------------------------------------------------------

if test "$MEASURE" = "1"
then
  run_measurements
fi

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

if test "$RAW" = "0"
then
    run_reports "$PACKAGES"
fi
