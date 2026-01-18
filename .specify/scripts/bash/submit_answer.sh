#!/bin/bash

# submit_answer.sh
# Submits answer to AOC, handling Part 1 vs Part 2 automatically

set -e

# Load session cookie from .env or get it
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/get_session_cookie.sh" 2>/dev/null || {
    AOC_SESSION=$("$SCRIPT_DIR/get_session_cookie.sh")
    export AOC_SESSION
}

YEAR=2025
DAY=${1:-$(date +%d | sed 's/^0//')}
ANSWER=$2
PART=${3:-""}

if [ -z "${AOC_SESSION:-}" ]; then
    AOC_SESSION=$("$SCRIPT_DIR/get_session_cookie.sh")
    export AOC_SESSION
fi

if [ -z "$ANSWER" ]; then
    echo "Usage: $0 <day> <answer> [part]"
    echo "  day: Day number (1-25)"
    echo "  answer: The answer to submit"
    echo "  part: '1' or '2' (optional - will auto-detect if not provided)"
    exit 1
fi

URL="https://adventofcode.com/${YEAR}/day/${DAY}/answer"

# If part not specified, try to detect it
if [ -z "$PART" ]; then
    # Check if we can determine part from previous submissions or README
    # For now, default to part 1 and let user specify
    echo "Warning: Part not specified. Defaulting to Part 1." >&2
    echo "  If you meant Part 2, run: $0 $DAY $ANSWER 2" >&2
    PART=1
fi

# Determine level (AOC uses "level" - 1 for Part 1, 2 for Part 2)
LEVEL=$PART

# Submit answer
echo "Submitting answer '$ANSWER' for Day ${DAY} Part ${PART}..."

RESPONSE=$(curl -s -X POST \
    -H "Cookie: session=${AOC_SESSION}" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d "level=${LEVEL}&answer=${ANSWER}" \
    "${URL}")

# Parse response to determine success/failure
if echo "$RESPONSE" | grep -qi "That's the right answer\|That's correct"; then
    echo "✓ Correct answer!"
    
    # Check if we unlocked Part 2
    if [ "$PART" = "1" ] && echo "$RESPONSE" | grep -qi "answer\|part two\|part 2"; then
        echo ""
        echo "Part 1 complete! Part 2 is now available."
    fi
    
    exit 0
elif echo "$RESPONSE" | grep -qi "That's not the right answer\|You gave an answer"; then
    echo "✗ Incorrect answer. Try again."
    exit 1
elif echo "$RESPONSE" | grep -qi "You don't seem to be solving\|not logged in"; then
    echo "Error: Authentication failed. Check your session cookie." >&2
    exit 1
elif echo "$RESPONSE" | grep -qi "already complete\|You have already solved"; then
    echo "ℹ This part is already complete."
    exit 0
else
    echo "Unknown response from AOC server."
    echo "Response preview:"
    echo "$RESPONSE" | head -n 20
    exit 1
fi
