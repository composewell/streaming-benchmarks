#!/usr/bin/env bash

set -o pipefail

SCRIPT_DIR=$(cd `dirname $0`; pwd)

BENCHMARK_DIR=.
RUNNING_BENCHMARKS=y
source $SCRIPT_DIR/build-lib.sh

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <"bench1 bench2 ..." | help>]"
  echo "       [--prefix <benchmark name prefix to match>"
  echo "       [--fields <"field1 field2 ..." | help>]"
  echo "       [--sort-by-name]"
  echo "       [--graphs]"
  echo "       [--no-measure]"
  echo "       [--append]"
  echo "       [--long]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--raw]"
  echo "       [--dev-build]"
  echo "       [--use-nix]"
  echo "       [--with-compiler <compiler exe name>]"
  echo "       [--cabal-build-options <options>]"
  echo "       [--rtsopts <opts>]"
  echo "       [--compare] [--base <commit>] [--candidate <commit>]"
  echo "       -- <gauge options or benchmarks>"
  echo
  echo "--benchmarks: benchmarks to run, use 'help' for list of benchmarks"
  echo "--fields: measurement fields to report, use 'help' for a list"
  echo "--graphs: Generate graphical reports"
  echo "--no-measure: Don't run benchmarks, run reports from previous results"
  echo "--append: Don't overwrite previous results, append for comparison"
  echo "--long: Use much longer stream size for infinite stream benchmarks"
  echo "--slow: Slightly more accurate results at the expense of speed"
  echo "--quick: Faster results, useful for longer benchmarks"
  echo "--raw: Run the benchmarks but don't report them. This is useful when"
  echo "       you only want to work with the csv files generated."
  echo "--cabal-build-options: Pass any cabal build options to be used for build"
  echo "       e.g. --cabal-build-options \"--flag dev\""
  echo
  echo "When specific space complexity group is chosen then (and only then) "
  echo "RTS memory restrictions are used accordingly. For example, "
  echo "bench.sh --benchmarks Data.Parser -- Data.Parser/o-1-space "
  echo "restricts Heap/Stack space for O(1) characterstics"
  echo
  echo "When using --compare, by default comparative chart of HEAD^ vs HEAD"
  echo "commit is generated, in the 'charts' directory."
  echo "Use --base and --candidate to select the commits to compare."
  echo
  echo "Any arguments after a '--' are passed directly to gauge"
  exit
}

#-----------------------------------------------------------------------------
# Reporting utility functions
#-----------------------------------------------------------------------------

list_comparisons ()  {
  echo "Comparison groups:"
  for i in $COMPARISONS
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

# chart is expensive to build and usually not required to be rebuilt
cabal_which_report() {
    local path="./bin/$1"
    if test -e "$path"
    then
      echo $path
    fi
}

find_report_prog() {
    local prog_name="bench-report"
    hash -r
    local prog_path=$(cabal_which_report $prog_name)
    if test -x "$prog_path"
    then
      echo $prog_path
    else
      return 1
    fi
}

build_report_prog() {
    local prog_name="bench-report"
    local prog_path=$(cabal_which_report $prog_name)

    hash -r
    if test ! -x "$prog_path" -a "$BUILD_ONCE" = "0"
    then
      echo "Building bench-report executables"
      BUILD_ONCE=1
      pushd $BENCHMARK_DIR/bench-report
      local cmd
      cmd="$CABAL_EXECUTABLE install --installdir ../../bin bench-report"
      if test "$USE_NIX" -eq 0
      then
        $cmd || die "bench-report build failed"
      else
        nix-shell --run "$cmd" || die "bench-report build failed"
      fi
      popd

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
          die "Cannot find bench-report executable"
      echo "Using bench-report executable [$prog]"
  fi
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.

bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

invoke_gauge () {
    local target_prog="$1"
    local target_name="$2"
    local output_file="$3"

    local MATCH=""
    if test "$LONG" -ne 0
    then
      MATCH="$target_name/o-1-space"
    else
      MATCH="$BENCH_PREFIX"
    fi
    echo "name,iters,time,cycles,cpuTime,utime,stime,maxrss,minflt,majflt,nvcsw,nivcsw,allocated,numGcs,bytesCopied,mutatorWallSeconds,mutatorCpuSeconds,gcWallSeconds,gcCpuSeconds" >> $output_file
    # keep only benchmark names with shortest prefix e.g. "a/b/c" and "a/b", we
    # should only keep "a/b" otherwise benchmarks will run multiple times. why?
    $target_prog -l \
      | grep "^$MATCH" \
      | while read -r name; \
  do bin/bench-exec-one.sh "$name" "${GAUGE_ARGS[@]}" || exit 1; done \
      || die "Benchmark execution failed."
}

invoke_tasty_bench () {
    local target_prog="$1"
    local target_name="$2"
    local output_file="$3"

    local MATCH=""
    if test "$LONG" -ne 0
    then
      MATCH="-p /$target_name\/o-1-space/"
    else
        if test -n "$BENCH_PREFIX"
        then
          # escape "/"
          local escaped_name=$(echo "$BENCH_PREFIX" | sed -e 's/\//\\\//g')
          MATCH="-p /$escaped_name/"
        fi
    fi
    echo "Name,cpuTime,2*Stdev (ps),Allocated,bytesCopied" >> $output_file
    $target_prog -l $MATCH \
      | grep "^All" \
      | while read -r name; \
          do bin/bench-exec-one.sh "$name" "${GAUGE_ARGS[@]}" || exit 1; done \
      || die "Benchmark execution failed."
}

run_bench_target () {
  local package_name=$1
  local component=$2
  local target_name=$3

  local target_prog
  target_prog=$(cabal_target_prog $package_name $component $target_name) || \
    die "Cannot find executable for target $target_name"

  echo "Running executable $target_name ..."

  # Needed by bench-exec-one.sh
  export BENCH_EXEC_PATH=$target_prog
  export RTS_OPTIONS
  export QUICK_MODE
  export USE_GAUGE
  export LONG

  local output_file=$(bench_output_file $target_name)
  mkdir -p `dirname $output_file`
  if test "$USE_GAUGE" -eq 0
  then invoke_tasty_bench "$target_prog" "$target_name" "$output_file"
  else invoke_gauge "$target_prog" "$target_name" "$output_file"
  fi
}

# $1: package name
# $2: component
# $3: targets
run_bench_targets() {
    for i in $3
    do
      run_bench_target $1 $2 $i
    done
}

run_benches_comparing() {
    local bench_list=$1

    if test -z "$CANDIDATE"
    then
      CANDIDATE=$(git rev-parse HEAD)
    fi
    if test -z "$BASE"
    then
      # XXX Should be where the current branch is forked from master
      BASE="$CANDIDATE^"
    fi
    echo "Comparing baseline commit [$BASE] with candidate [$CANDIDATE]"
    echo "Checking out base commit [$BASE] for benchmarking"
    git checkout "$BASE" || die "Checkout of base commit [$BASE] failed"

    $BUILD_BENCH || die "build failed"
    run_bench_targets streamly-benchmarks b "$bench_list" target_exe_extra_args

    echo "Checking out candidate commit [$CANDIDATE] for benchmarking"
    git checkout "$CANDIDATE" || \
        die "Checkout of candidate [$CANDIDATE] commit failed"

    $BUILD_BENCH || die "build failed"
    run_bench_targets streamly-benchmarks b "$bench_list" target_exe_extra_args
    # XXX reset back to the original commit
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

  for i in $bench_list
  do
      backup_output_file $i
  done

  if test "$COMPARE" = "0"
  then
    run_bench_targets streamly-benchmarks b "$bench_list" target_exe_extra_args
  else
    run_benches_comparing "$bench_list"
  fi
}

run_reports() {
    local prog
    prog=$(find_report_prog) || \
      die "Cannot find bench-graph executable"
    echo

    for i in $1
    do
        echo "Generating reports for ${i}..."
        $prog \
            --benchmark $i \
            $(test "$GRAPH" = 1 && echo "--graphs") \
            $(test "$SORT_BY_NAME" = 1 && echo "--sort-by-name") \
            --fields "$FIELDS"
    done
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

cd $SCRIPT_DIR/..

USE_GAUGE=0
USE_GIT_CABAL=1
USE_NIX=0
set_common_vars

DEFAULT_FIELDS="allocated bytescopied cputime"
ALL_FIELDS="$FIELDS time cycles utime stime minflt majflt nvcsw nivcsw"
FIELDS=$DEFAULT_FIELDS

COMPARE=0
BASE=
CANDIDATE=

APPEND=0
LONG=0
RAW=0
SORT_BY_NAME=0
GRAPH=0
MEASURE=1

GAUGE_ARGS=
BUILD_ONCE=0

CABAL_BUILD_OPTIONS="--flag fusion-plugin "

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --benchmarks) shift; TARGETS=$1; shift ;;
    --targets) shift; TARGETS=$1; shift ;;
    --prefix) shift; BENCH_PREFIX="$1"; shift ;;
    --fields) shift; FIELDS=$1; shift ;;
    --base) shift; BASE=$1; shift ;;
    --candidate) shift; CANDIDATE=$1; shift ;;
    --with-compiler) shift; CABAL_WITH_COMPILER=$1; shift ;;
    --cabal-build-flags) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --cabal-build-options) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --rtsopts) shift; RTS_OPTIONS=$1; shift ;;
    # flags
    --slow) SLOW=1; shift ;;
    --quick) QUICK_MODE=1; shift ;;
    --compare) COMPARE=1; shift ;;
    --raw) RAW=1; shift ;;
    --append) APPEND=1; shift ;;
    --long) LONG=1; shift ;;
    --sort-by-name) SORT_BY_NAME=1; shift ;;
    --graphs) GRAPH=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --dev-build) RUNNING_DEVBUILD=1; shift ;;
    --use-nix) USE_NIX=1; shift ;;
    --use-gauge) USE_GAUGE=1; shift ;;
    --) shift; break ;;
    *) echo "Unknown flags: $*"; echo; print_help ;;
  esac
done
GAUGE_ARGS=("$@")

set_derived_vars

#-----------------------------------------------------------------------------
# Determine targets
#-----------------------------------------------------------------------------

only_real_benchmarks () {
  for i in $TARGETS
  do
    local SKIP=0
    for j in $COMPARISONS
    do
      if test $i == $j
      then
        SKIP=1
      fi
    done
    if test "$SKIP" -eq 0
    then
      echo -n "$i "
    fi
  done
}

# Requires RUNNING_DEVBUILD var
source $SCRIPT_DIR/targets.sh

if test "$(has_item "$TARGETS" help)" = "help"
then
  list_target_groups
  list_comparisons
  list_targets
  exit
fi

if test "$(has_item "$FIELDS" help)" = "help"
then
  echo "Supported fields: $ALL_FIELDS"
  echo "Default fields: $DEFAULT_FIELDS"
  exit
fi

if test "$LONG" -ne 0
then
  if test -n "$TARGETS"
  then
    echo "Cannot specify benchmarks [$TARGETS] with --long"
    exit
  fi
  TARGETS=$infinite_grp
fi

DEFAULT_TARGETS="$(all_grp)"
TARGETS=$(set_targets)

TARGETS_ORIG=$TARGETS
TARGETS=$(only_real_benchmarks)

echo "Using benchmark suites [$TARGETS]"

#-----------------------------------------------------------------------------
# Build reporting utility
#-----------------------------------------------------------------------------

# We need to build the report progs first at the current (latest) commit before
# checking out any other commit for benchmarking.
build_report_progs

#-----------------------------------------------------------------------------
# Build and run targets
#-----------------------------------------------------------------------------

if test "$USE_GAUGE" -eq 1
then
  BUILD_FLAGS="--flag use-gauge"
fi

BUILD_BENCH="$CABAL_EXECUTABLE v2-build $BUILD_FLAGS $CABAL_BUILD_OPTIONS --enable-benchmarks"
if test "$MEASURE" = "1"
then
  run_build "$BUILD_BENCH" streamly-benchmarks bench "$TARGETS"
  run_measurements "$TARGETS"
fi

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

COMPARISON_REPORTS=""
for i in $COMPARISONS
do
  if test "$(has_item "$TARGETS_ORIG" $i)" = $i
  then
    COMPARISON_REPORTS="$COMPARISON_REPORTS $i"
    mkdir -p "charts/$i"
    constituents=$(eval "echo -n \$${i}")
    dest_file="charts/$i/results.csv"
    : > $dest_file
    for j in $constituents
    do
      cat "charts/$j/results.csv" >> $dest_file
    done
  fi
done

if test "$RAW" = "0"
then
  run_reports "$TARGETS"
  run_reports "$COMPARISON_REPORTS"
fi
