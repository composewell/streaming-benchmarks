#!/bin/bash

print_help () {
  echo "Usage: $0 [--quick] [--pedantic] <benchmark-name or prefix> [min-samples]"
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
    *) break ;;
  esac
done

MIN_SAMPLES=1
if test -n "$2"
then
  MIN_SAMPLES=$2
fi

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to guage as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.
# Use this command to find the exe if this script fails with an error:
# find .stack-work/ -type f -name "benchmarks"

os=$(uname)
case "$os" in
  Linux)
    BENCH_EXE=.stack-work/dist/x86_64-linux-nopie/Cabal-2.0.1.0/build/benchmarks/benchmarks
    ;;
  Darwin)
    BENCH_EXE=.stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/benchmarks/benchmarks
    ;;
  MINGW*)
    die "OS: Windows (MINGW). Please edit this script to set the correct path of the benchmark executable here" ;;
  *) die "OS: Unknown OS [$os]" ;;
esac

PROG="--measure-with $BENCH_EXE"

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

if test "$PEDANTIC" = "1"
then
  GHC_PATH=`stack path --compiler-bin`
  export PATH=$GHC_PATH:$PATH
  mkdir -p .stack-root
  export STACK_ROOT=`pwd`/.stack-root
  STACK_OPTIONS="--system-ghc --stack-yaml stack-pedantic.yaml"
fi

stack $STACK_OPTIONS bench --benchmark-arguments "$ARGS \
  --include-first-iter \
  --min-duration 0 \
  --min-samples $MIN_SAMPLES \
  -v 2 \
  $PROG $1"
