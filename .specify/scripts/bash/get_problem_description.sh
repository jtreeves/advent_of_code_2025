#!/bin/bash

# get_problem_description.sh
# Fetches problem description from AOC and updates README.md
# Handles Part 1 initially, then Part 2 when available

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

README_FILE="days/day_${DAY_PADDED}/README.md"
URL="https://adventofcode.com/${YEAR}/day/${DAY}"

# Fetch HTML page
HTML_FILE=$(mktemp)
curl -s -H "Cookie: session=${AOC_SESSION}" "${URL}" > "$HTML_FILE"

if [ $? -ne 0 ]; then
    echo "Error: Failed to fetch problem description" >&2
    rm -f "$HTML_FILE"
    exit 1
fi

# Extract problem description (using grep and sed, or Python if available)
# AOC pages have the problem in <article> tags with class "day-desc"

if command -v python3 &> /dev/null; then
    # Use Python for more reliable HTML parsing
    DESCRIPTION=$(python3 << EOF
import sys
import re
from html import unescape

with open("$HTML_FILE", "r", encoding="utf-8") as f:
    content = f.read()

# Extract all article tags
articles = re.findall(r'<article class="day-desc">(.*?)</article>', content, re.DOTALL)

parts = []
for article in articles:
    # Remove HTML tags but keep text
    text = re.sub(r'<[^>]+>', '', article)
    text = unescape(text)
    text = re.sub(r'\n\s*\n', '\n\n', text)  # Normalize whitespace
    text = text.strip()
    if text:
        parts.append(text)

# Join parts
result = '\n\n---\n\n'.join(parts)
print(result)
EOF
)
else
    # Fallback: simple extraction (less reliable)
    DESCRIPTION=$(grep -A 1000 '<article class="day-desc">' "$HTML_FILE" | \
        grep -B 1000 '</article>' | \
        sed 's/<[^>]*>//g' | \
        sed 's/&lt;/</g; s/&gt;/>/g; s/&amp;/\&/g' | \
        sed 's/^\s*//g' | \
        sed '/^$/N;/^\n$/d')
fi

rm -f "$HTML_FILE"

if [ -z "$DESCRIPTION" ]; then
    echo "Warning: Could not extract problem description. The page may not be available yet." >&2
    exit 1
fi

# Check if README exists
if [ ! -f "$README_FILE" ]; then
    echo "Error: README file not found at $README_FILE" >&2
    exit 1
fi

# Determine which part we have
PART_COUNT=$(echo "$DESCRIPTION" | grep -c "^---" || echo "0")
HAS_PART_2=$([ "$PART_COUNT" -ge 1 ] && echo "yes" || echo "no")

# Read current README
README_CONTENT=$(cat "$README_FILE")

# Replace the problem description section
# Look for markers like "## Problem Overview" or "## Part 1"
if echo "$README_CONTENT" | grep -q "## Problem Overview\|## Part 1"; then
    # Update existing content between "## Problem Overview" or start and "## Key Info" or "## Usage"
    if echo "$README_CONTENT" | grep -q "## Key Info"; then
        # Extract everything before "## Problem Overview" or start, and after "## Key Info"
        BEFORE=$(echo "$README_CONTENT" | sed '/## Problem Overview\|^## Part 1/q' | head -n -1)
        AFTER=$(echo "$README_CONTENT" | sed -n '/## Key Info/,$p')
        
        # Format the description
        if [ "$HAS_PART_2" = "yes" ]; then
            FORMATTED_DESC="## Problem Overview\n\n${DESCRIPTION}\n"
        else
            # Split description if it contains "---" marker
            PART1=$(echo "$DESCRIPTION" | sed '/^---/,$d')
            FORMATTED_DESC="## Problem Overview\n\n${PART1}\n\n## Part 1\n\n${PART1}\n"
        fi
        
        NEW_README="${BEFORE}${FORMATTED_DESC}${AFTER}"
    else
        # No "Key Info" section, append at end before Usage
        if echo "$README_CONTENT" | grep -q "## Usage"; then
            BEFORE=$(echo "$README_CONTENT" | sed '/## Usage/q' | head -n -1)
            AFTER=$(echo "$README_CONTENT" | sed -n '/## Usage/,$p')
            
            if [ "$HAS_PART_2" = "yes" ]; then
                FORMATTED_DESC="\n## Problem Overview\n\n${DESCRIPTION}\n"
            else
                PART1=$(echo "$DESCRIPTION" | sed '/^---/,$d')
                FORMATTED_DESC="\n## Problem Overview\n\n${PART1}\n"
            fi
            
            NEW_README="${BEFORE}${FORMATTED_DESC}${AFTER}"
        else
            # Just append at the top
            NEW_README="## Problem Overview\n\n${DESCRIPTION}\n\n${README_CONTENT}"
        fi
    fi
else
    # No existing problem section, insert at the top after day title
    if echo "$README_CONTENT" | grep -q "^# Day"; then
        TITLE=$(echo "$README_CONTENT" | head -n 1)
        REST=$(echo "$README_CONTENT" | tail -n +2)
        NEW_README="${TITLE}\n\n## Problem Overview\n\n${DESCRIPTION}\n\n${REST}"
    else
        # No title either, just prepend
        NEW_README="## Problem Overview\n\n${DESCRIPTION}\n\n${README_CONTENT}"
    fi
fi

# Write updated README
echo -e "$NEW_README" > "$README_FILE"

echo "Updated README.md for Day ${DAY}"
if [ "$HAS_PART_2" = "yes" ]; then
    echo "  - Both Part 1 and Part 2 descriptions included"
else
    echo "  - Part 1 description included (Part 2 not yet available)"
fi
