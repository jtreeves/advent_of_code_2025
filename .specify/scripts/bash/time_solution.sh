#!/bin/bash

# time_solution.sh
# Measures execution time of a solution in milliseconds
# Usage: time_solution.sh <day_number> <language> [solution_file]
# Language: c, clj, ex, go, hs, java, jl, kt, py, rb, rs, ts

set -e

DAY=${1:-$(date +%d | sed 's/^0//')}
DAY_PADDED=$(printf "%02d" "$DAY")
LANG=$2
SOLUTION_FILE=${3:-"solution.${LANG}"}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOLUTION_DIR="${SCRIPT_DIR}/../../../days/day_${DAY_PADDED}/solutions"
cd "$SOLUTION_DIR"

# Function to get current time in milliseconds (Unix timestamp * 1000)
get_time_ms() {
    if command -v gdate >/dev/null 2>&1; then
        # macOS with GNU date installed
        gdate +%s%3N
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS - use Python for millisecond precision
        python3 -c "import time; print(int(time.time() * 1000))"
    else
        # Linux
        date +%s%3N
    fi
}

# Function to measure execution time
measure_time() {
    local start_ms=$(get_time_ms)
    "$@" > /dev/null 2>&1
    local end_ms=$(get_time_ms)
    echo $((end_ms - start_ms))
}

# Run solution based on language
case "$LANG" in
    py)
        PYTHONPATH="${SCRIPT_DIR}/../../../utilities/python:$PYTHONPATH" \
        measure_time python3 "$SOLUTION_FILE"
        ;;
    go)
        measure_time go run "$SOLUTION_FILE"
        ;;
    rs)
        TEMP_BIN="sol_rs_$$"
        rustc "$SOLUTION_FILE" -o "$TEMP_BIN" 2>/dev/null
        measure_time "./$TEMP_BIN"
        rm -f "$TEMP_BIN"
        ;;
    java)
        javac "$SOLUTION_FILE" 2>/dev/null
        measure_time java Solution
        rm -f Solution.class
        ;;
    ts)
        measure_time npx ts-node "$SOLUTION_FILE"
        ;;
    rb)
        measure_time ruby "$SOLUTION_FILE"
        ;;
    kt)
        TEMP_JAR="sol_kt_$$.jar"
        kotlinc "$SOLUTION_FILE" -include-runtime -d "$TEMP_JAR" 2>/dev/null
        measure_time java -jar "$TEMP_JAR"
        rm -f "$TEMP_JAR"
        ;;
    jl)
        measure_time julia "$SOLUTION_FILE"
        ;;
    c)
        TEMP_BIN="sol_c_$$"
        gcc -o "$TEMP_BIN" "$SOLUTION_FILE" "${SCRIPT_DIR}/../../../utilities/c/get_input.c" 2>/dev/null
        measure_time "./$TEMP_BIN"
        rm -f "$TEMP_BIN"
        ;;
    clj)
        measure_time clojure -M "$SOLUTION_FILE"
        ;;
    ex)
        measure_time elixir "$SOLUTION_FILE"
        ;;
    hs)
        TEMP_BIN="sol_hs_$$"
        ghc -o "$TEMP_BIN" "$SOLUTION_FILE" >/dev/null 2>&1
        measure_time "./$TEMP_BIN"
        rm -f "$TEMP_BIN" "${SOLUTION_FILE%.hs}.hi" "${SOLUTION_FILE%.hs}.o"
        ;;
    *)
        echo "Error: Unknown language: $LANG" >&2
        exit 1
        ;;
esac
