#!/bin/bash

print_help () {
  echo "Usage: $0 [--quick] [--pedantic] [--no-graph] [--no-measure] <benchmark-name or prefix> [min-samples]"
  exit
}

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    --quick) QUICK=1; shift ;;
    --pedantic) PEDANTIC=1; shift ;;
    --no-graph) GRAPH=0; shift ;;
    --no-measure) MEASURE=0; shift ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done

MIN_SAMPLES=1
if test -n "$2"
then
  MIN_SAMPLES=$2
fi

STACK=stack
if test "$PEDANTIC" = "1"
then
  GHC_PATH=`$STACK path --compiler-bin`
  export PATH=$GHC_PATH:$PATH
  mkdir -p .stack-root
  export STACK_ROOT=`pwd`/.stack-root
  STACK="$STACK --system-ghc --stack-yaml stack-pedantic.yaml"
fi

echo "Using stack command [$STACK]"
$STACK build --bench --no-run-benchmarks || die "build failed"

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to guage as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.
# Use this command to find the exe if this script fails with an error:
# find .stack-work/ -type f -name "benchmarks"

enable_isolated () {
  local PROG=`$STACK path --dist-dir`/build/benchmarks/benchmarks
  if test -x "$PROG"
  then
    BENCH_PROG="--measure-with $PROG"
  else
    echo
    echo "WARNING! benchmark binary [$PROG] not found or not executable"
    echo "WARNING! not using isolated measurement."
    echo
  fi
}

enable_isolated

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

if test "$QUICK" = "1"
then
  ARGS="--quick"
fi

if test "$MEASURE" != "0"
  then
  if test -e results.csv
  then
    mv -f -v results.csv results.csv.prev
  fi

  $STACK bench --benchmark-arguments "$ARGS \
    --include-first-iter \
    --min-duration 0 \
    --min-samples $MIN_SAMPLES \
    --csv=results.csv \
    -v 2 \
    $BENCH_PROG $1" || die "Benchmarking failed"
fi

if test "$GRAPH" != "0"
then
  echo
  echo "Generating charts from results.csv..."
  $STACK exec makecharts results.csv
fi
