#!/bin/bash

# get_input.sh
# Fetches input for a specific day from AOC and overwrites input.txt

set -e

# Load session cookie from .env or get it
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/get_session_cookie.sh" 2>/dev/null || {
    AOC_SESSION=$("$SCRIPT_DIR/get_session_cookie.sh")
    export AOC_SESSION
}

YEAR=2025
DAY=${1:-$(date +%d | sed 's/^0//')}
DAY_PADDED=$(printf "%02d" "$DAY")

if [ -z "${AOC_SESSION:-}" ]; then
    AOC_SESSION=$("$SCRIPT_DIR/get_session_cookie.sh")
    export AOC_SESSION
fi

INPUT_FILE="days/day_${DAY_PADDED}/data/input.txt"
URL="https://adventofcode.com/${YEAR}/day/${DAY}/input"

# Create directory if it doesn't exist
mkdir -p "$(dirname "$INPUT_FILE")"

# Fetch input
echo "Fetching input for Day ${DAY}..."
curl -s -H "Cookie: session=${AOC_SESSION}" "${URL}" > "$INPUT_FILE"

if [ $? -ne 0 ]; then
    echo "Error: Failed to fetch input for Day ${DAY}" >&2
    exit 1
fi

# Check if we got HTML (which usually means an error page)
if head -n 1 "$INPUT_FILE" | grep -q "<!DOCTYPE\|<html"; then
    echo "Error: Received HTML instead of input. Check your session cookie or if the day is available." >&2
    rm -f "$INPUT_FILE"
    exit 1
fi

# Remove trailing newline if present
sed -i.bak -e :a -e '/^\n*$/{$d;N;ba' -e '}' "$INPUT_FILE" 2>/dev/null || {
    # Fallback for systems where sed -i behaves differently
    local content=$(cat "$INPUT_FILE")
    printf '%s' "$content" | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}' > "$INPUT_FILE"
}
rm -f "${INPUT_FILE}.bak" 2>/dev/null || true

echo "Successfully fetched input for Day ${DAY}"
echo "Saved to $INPUT_FILE"
