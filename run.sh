#!/bin/bash

print_help () {
  echo "Usage: $0 [--quick] [--append] [--pedantic] [--no-graphs] [--no-measure] -- <gauge options>"
  echo "Any arguments after a '--' are passed directly to guage"
  echo "You can omit '--' if the gauge args used do not start with a '-'."
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
    --append) APPEND=1; shift ;;
    --pedantic) PEDANTIC=1; shift ;;
    --no-graphs) GRAPH=0; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --) shift; break ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done

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
  ENABLE_QUICK="--quick"
fi

if test "$MEASURE" != "0"
  then
  if test -e results.csv -a "$APPEND" != 1
  then
    mv -f -v results.csv results.csv.prev
  fi

  # We set min-samples to 3 if we use less than three samples, statistical
  # analysis crashes. Note that the benchmark runs for a minimum of 5 seconds.
  # We use min-duration=0 to run just one iteration for each sample, we anyway
  # run a million ops in each iteration so we do not need more iterations.
  # However with fusion, million ops finish in microseconds. The
  # default is to run iterations worth minimum 30 ms and most of our benchmarks
  # are close to that or more.
  #  --min-duration 0 \
  $STACK bench --benchmark-arguments "$ENABLE_QUICK \
    --include-first-iter \
    --min-samples 3 \
    --match exact \
    --csvraw=results.csv \
    -v 2 \
    $BENCH_PROG $*" || die "Benchmarking failed"
fi

if test "$GRAPH" != "0"
then
  echo
  echo "Generating charts from results.csv..."
  $STACK exec makecharts results.csv
fi
