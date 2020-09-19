#!/bin/bash

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <streamly,vector,...>]"
  echo "       [--diff <percent|multiples>]"
  echo "       [--graphs]"
  echo "       [--measure]"
  echo "       [--append] "
  echo "       [--slow]"
  echo "       [--fast]"
  echo "       [--versions] "
  echo "       [--stack] "
  echo "       -- <gauge options>"
  echo
  echo "--benchmarks: specify comma separated list of packages to be compared"
  echo
  echo "Available benchmarks are: "
  echo "Pure streams: list, pure-streamly, dlist, sequence"
  echo "Monadic streams: streamly, streams-vector, streaming, pipes, conduit, machines, drinkery"
  echo "Arrays: array-streamly, bytestring, lazy-bytestring, text, vector, storable-vector, unboxed-vector"
  echo
  echo "--graphs: generate SVG graphs instead of text reports"
  echo "--diff: show diff of subsequent packages from the first package"
  echo "--slow: slower but a bit more precise benchmarking"
  echo "--fast: faster but a bit less precise benchmarking"
  echo "--measure: rerun benchmark measurements"
  echo "--append: append the new measurement results to previous ones for comparison"
  echo "--versions: add package versions in the report/graphs"
  echo "--stack: use stack for build and benchmark"
  echo
  echo "Any arguments after a '--' are passed directly to guage"
  exit
}

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

set_benchmarks() {
  if test -z "$BENCHMARKS"
  then
    BENCHMARKS=$DEFAULT_BENCHMARKS
  elif test "$BENCHMARKS" = "all"
  then
    BENCHMARKS=$ALL_BENCHMARKS
  fi
  echo "Using benchmark suites [$BENCHMARKS]"
}

# $1: benchmark name (linear, nested, base)
find_report_prog() {
    local prog_name="bench-report"
    hash -r
    local prog_path=$($WHICH_COMMAND $prog_name)
    if test -x "$prog_path"
    then
      echo $prog_path
    else
      if test -x "charts/$prog_name"
      then echo "charts/$prog_name"
      else
        return 1
      fi
    fi
}

# $1: benchmark name (linear, nested, base)
build_report_prog() {
    local prog_name="bench-report"
    local prog_path=$($WHICH_COMMAND $prog_name)

    hash -r
    if test -x "charts/$prog_name"
    then return 0
    fi
    if test ! -x "$prog_path" -a "$BUILD_ONCE" = "0"
    then
      echo "Building bench-show executable"
      BUILD_ONCE=1
      $BUILD_CHART_EXE || die "build failed"
      if test "$USE_STACK" -ne 1
      then
        cabal install --flag dev --installdir=charts bench-report
      fi
    elif test ! -x "$prog_path"
    then
      return 1
    fi
    return 0
}

build_report_progs() {
  if test "$RAW" = "0"
  then
      build_report_prog || exit 1
      local prog
      prog=$(find_report_prog) || \
          die "Cannot find bench-show executable"
      echo "Using bench-show executable [$prog]"
  fi
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to guage as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.
# Use this command to find the exe if this script fails with an error:
# find .stack-work/ -type f -name "benchmarks"

stack_bench_prog () {
  local bench_name=$1
  local bench_prog=`stack path --dist-dir`/build/$bench_name/$bench_name
  if test -x "$bench_prog"
  then
    echo $bench_prog
  else
    return 1
  fi
}

cabal_bench_prog () {
  local bench_name=$1
  local bench_prog=`$WHICH_COMMAND $1`
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
  local bench_name=$1
  local output_file=$(bench_output_file $bench_name)
  local bench_prog
  bench_prog=$($GET_BENCH_PROG $bench_name) || \
    die "Cannot find benchmark executable for benchmark $bench_name"

  mkdir -p `dirname $output_file`

  echo "Running benchmark $bench_name ..."

  local MATCH_ARGS=""
  for i in $(echo $BENCHMARKS | tr "," "\n")
  do
     MATCH_ARGS="$MATCH_ARGS -m pattern /$i"
  done

  $bench_prog $SPEED_OPTIONS \
    --csvraw=$output_file \
    -v 2 \
    --measure-with $bench_prog $MATCH_ARGS $GAUGE_ARGS \
      || die "Benchmarking failed"
}

run_benches() {
    for i in $1
    do
      run_bench $i
    done
}

backup_output_file() {
  local bench_name=$1
  local output_file=$(bench_output_file $bench_name)

  if test -e $output_file -a "$APPEND" != 1
  then
      mv -f -v $output_file ${output_file}.prev
  fi
}

run_measurements() {
  local bench_list=$1
  local to_run

  for i in $bench_list
  do
    local output_file=$(bench_output_file $i)
    if test "$MEASURE" = 1 -o ! -e $output_file
    then
      backup_output_file $i
      to_run="$to_run $i"
    fi
  done

  run_benches "$to_run"
}

run_reports() {
    local prog
    prog=$(find_report_prog) || \
      die "Cannot find bench-graph executable"
    echo

    local output_file=$(bench_output_file)
    echo "Generating reports for ${output_file}..."
    $prog $output_file $1 $GRAPH $DELTA $VERSIONS
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

DEFAULT_BENCHMARKS="streamly,streaming,conduit,pipes,machines"
ALL_BENCHMARKS="streamly,vector,streaming,conduit,pipes,machines,drinkery"
DELTA="absolute"

APPEND=0
RAW=0
GRAPH=False
VERSIONS=False
MEASURE=0
SPEED_OPTIONS="--quick --min-samples 10 --time-limit 1 --min-duration 0"

GAUGE_ARGS=
BUILD_ONCE=0
USE_STACK=0

GHC_VERSION=$(ghc --numeric-version)
STREAMING_BENCHMARKS_VERSION=0.3.0

cabal_which() {
  find dist-newstyle -type f -path \
  "*${GHC_VERSION}/streaming-benchmarks-$STREAMING_BENCHMARKS_VERSION/*/$1"
}

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --slow) SPEED_OPTIONS="--min-duration 0"; shift ;;
    --fast) SPEED_OPTIONS="--quick --min-samples 1 --min-duration 0 --include-first-iter"
            shift ;;
    --append) APPEND=1; shift ;;
    --benchmarks) shift; BENCHMARKS=$1; shift ;;
    --diff) shift; DELTA=$1; shift ;;
    --raw) RAW=1; shift ;;
    --graphs) GRAPH=True; shift ;;
    --versions) VERSIONS=True; shift ;;
    --stack) USE_STACK=1; shift ;;
    --measure) MEASURE=1; shift ;;
    --) shift; break ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done
GAUGE_ARGS=$*

if test "$USE_STACK" = "1"
then
  WHICH_COMMAND="stack exec which"
  GET_BENCH_PROG=stack_bench_prog
  BUILD_CHART_EXE="stack build"
  BUILD_BENCH="stack build $STACK_BUILD_FLAGS --bench --no-run-benchmarks"
else
  # XXX cabal issue "cabal v2-exec which" cannot find benchmark/test executables
  #WHICH_COMMAND="cabal v2-exec which"
  WHICH_COMMAND=cabal_which
  GET_BENCH_PROG=cabal_bench_prog
  BUILD_CHART_EXE="cabal v2-build --flag dev bench-report"
  BUILD_BENCH="cabal v2-build $CABAL_BUILD_FLAGS --enable-benchmarks all"
fi

# echo "Using stack command [$STACK]"
set_benchmarks

#-----------------------------------------------------------------------------
# Build stuff
#-----------------------------------------------------------------------------

# We need to build the report progs first at the current (latest) commit before
# checking out any other commit for benchmarking.
#build_report_progs "$BENCHMARKS"

if test ! -e charts/bench-report
then
  echo "Please build the bench-report executable first."
  echo "It requires ghc-8.8.4 or earlier"
  echo
  echo "cabal install --flag dev --installdir charts --with-compiler ghc-8.8.4 bench-report"
  exit
fi

#-----------------------------------------------------------------------------
# Run benchmarks
#-----------------------------------------------------------------------------

$BUILD_BENCH || die "build failed"
run_measurements bmarks

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

if test "$RAW" = "0"
then
  run_reports "$BENCHMARKS"
fi
