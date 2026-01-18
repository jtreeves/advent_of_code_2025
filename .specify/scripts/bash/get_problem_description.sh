#!/bin/bash

# get_problem_description.sh
# Fetches problem description from AOC and updates README.md
# Can handle Part 1 only, both parts, or just Part 2 (for later updates)

set -e

# Load session cookie from .env or get it
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/get_session_cookie.sh" 2>/dev/null || {
    AOC_SESSION=$("$SCRIPT_DIR/get_session_cookie.sh")
    export AOC_SESSION
}

YEAR=2025
DAY=${1:-$(date +%d | sed 's/^0//')}
PART_ONLY=${2:-""}  # Optional: "1" or "2" to fetch only that part
DAY_PADDED=$(printf "%02d" "$DAY")

if [ -z "${AOC_SESSION:-}" ]; then
    AOC_SESSION=$("$SCRIPT_DIR/get_session_cookie.sh")
    export AOC_SESSION
fi

README_FILE="days/day_${DAY_PADDED}/README.md"
URL="https://adventofcode.com/${YEAR}/day/${DAY}"

# Create README from template if it doesn't exist
if [ ! -f "$README_FILE" ]; then
    mkdir -p "$(dirname "$README_FILE")"
    # Use template structure
    cat > "$README_FILE" << EOF
# Day ${DAY_PADDED}

[[Day ${DAY_PADDED}]](https://adventofcode.com/${YEAR}/day/${DAY})

## Part 1

### Problem
[Part 1 description will be extracted from AOC page]

### Solution
[Answer will be populated after running solutions]

## Part 2

### Problem
[Part 2 description will be added when available]

### Solution
[Answer will be populated after running solutions]
EOF
fi

# Fetch HTML page
HTML_FILE=$(mktemp)
curl -s -H "Cookie: session=${AOC_SESSION}" "${URL}" > "$HTML_FILE"

if [ $? -ne 0 ]; then
    echo "Error: Failed to fetch problem description" >&2
    rm -f "$HTML_FILE"
    exit 1
fi

# Extract problem description (using Python if available)
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

# Join parts with separator
result = '\n\n---\n\n'.join(parts)
print(result)
EOF
)
    # Extract day title from HTML
    DAY_TITLE=$(python3 << EOF
import re
with open("$HTML_FILE", "r", encoding="utf-8") as f:
    content = f.read()
# Extract title from <h2> tags, usually "Day N: Title"
match = re.search(r'<h2[^>]*>(.*?)</h2>', content, re.DOTALL)
if match:
    title = re.sub(r'<[^>]+>', '', match.group(1))
    print(title.strip())
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
    DAY_TITLE="Day ${DAY_PADDED}"
fi

rm -f "$HTML_FILE"

if [ -z "$DESCRIPTION" ]; then
    echo "Warning: Could not extract problem description. The page may not be available yet." >&2
    exit 1
fi

# Split description into parts (separated by ---)
PART1=$(echo "$DESCRIPTION" | sed '/^---/,$d' | sed '/^$/d')
PART2=$(echo "$DESCRIPTION" | sed '1,/^---/d' | sed '/^$/d')

# Determine what we have
HAS_PART_2=$([ -n "$PART2" ] && echo "yes" || echo "no")

# Read current README into temporary file for processing
TEMP_README=$(mktemp)
cat "$README_FILE" > "$TEMP_README"

# Function to update Part 1 description using a temporary Python script
update_part1_in_place() {
    python3 << EOF
import sys
import re

with open("$TEMP_README", "r", encoding="utf-8") as f:
    content = f.read()

# Replace content in Part 1 > Problem section
part1_desc = """$PART1"""

# Pattern: ## Part 1 ... ### Problem ... ### Solution
pattern = r'(## Part 1\s+)(### Problem\s+)(.*?)(\s+### Solution)'
replacement = r'\1\2' + part1_desc + r'\4'

content = re.sub(pattern, replacement, content, flags=re.DOTALL)

with open("$TEMP_README", "w", encoding="utf-8") as f:
    f.write(content)
EOF
}

# Function to update Part 2 description using a temporary Python script
update_part2_in_place() {
    python3 << EOF
import sys
import re

with open("$TEMP_README", "r", encoding="utf-8") as f:
    content = f.read()

# Replace content in Part 2 > Problem section
part2_desc = """$PART2"""

# Pattern: ## Part 2 ... ### Problem ... ### Solution
pattern = r'(## Part 2\s+)(### Problem\s+)(.*?)(\s+### Solution)'
replacement = r'\1\2' + part2_desc + r'\4'

content = re.sub(pattern, replacement, content, flags=re.DOTALL)

with open("$TEMP_README", "w", encoding="utf-8") as f:
    f.write(content)
EOF
}

# Update README based on PART_ONLY parameter
if command -v python3 &> /dev/null; then
    if [ "$PART_ONLY" = "2" ]; then
        # Only update Part 2
        if [ "$HAS_PART_2" != "yes" ]; then
            echo "Warning: Part 2 not yet available on AOC" >&2
            rm -f "$TEMP_README"
            exit 1
        fi
        update_part2_in_place
        echo "Updated README.md for Day ${DAY} - Part 2 description added"
    elif [ "$PART_ONLY" = "1" ]; then
        # Only update Part 1
        update_part1_in_place
        echo "Updated README.md for Day ${DAY} - Part 1 description updated"
    else
        # Update both parts (default behavior)
        update_part1_in_place
        if [ "$HAS_PART_2" = "yes" ]; then
            update_part2_in_place
            echo "Updated README.md for Day ${DAY}"
            echo "  - Both Part 1 and Part 2 descriptions included"
        else
            echo "Updated README.md for Day ${DAY}"
            echo "  - Part 1 description included (Part 2 not yet available)"
        fi
    fi
    
    # Update day title link if we extracted it
    if [ -n "$DAY_TITLE" ] && [ "$DAY_TITLE" != "Day ${DAY_PADDED}" ]; then
        python3 << EOF
import re
with open("$TEMP_README", "r", encoding="utf-8") as f:
    content = f.read()
content = re.sub(r'\[\[Day.*?\]\]', f'[[{DAY_TITLE}]]', content)
with open("$TEMP_README", "w", encoding="utf-8") as f:
    f.write(content)
EOF
    fi
    
    # Move temp file to final location
    mv "$TEMP_README" "$README_FILE"
else
    # Fallback without Python - simpler sed-based approach
    echo "Warning: Python 3 not available, using basic replacement" >&2
    if [ "$PART_ONLY" = "2" ]; then
        if [ "$HAS_PART_2" != "yes" ]; then
            echo "Warning: Part 2 not yet available on AOC" >&2
            rm -f "$TEMP_README"
            exit 1
        fi
        # Simple sed replacement for Part 2 (may not work perfectly)
        sed -i.bak "/^## Part 2$/,/^### Solution$/{ /^### Problem$/,/^### Solution$/{ /^### Solution$/!d; } }" "$TEMP_README"
        sed -i.bak "/^### Problem$/a\\
${PART2}
" "$TEMP_README"
        rm -f "${TEMP_README}.bak"
        mv "$TEMP_README" "$README_FILE"
        echo "Updated README.md for Day ${DAY} - Part 2 description added (basic mode)"
    elif [ "$PART_ONLY" = "1" ]; then
        # Simple sed replacement for Part 1
        sed -i.bak "/^## Part 1$/,/^## Part 2$/{ /^### Problem$/,/^### Solution$/{ /^### Solution$/!d; } }" "$TEMP_README"
        sed -i.bak "/^### Problem$/a\\
${PART1}
" "$TEMP_README"
        rm -f "${TEMP_README}.bak"
        mv "$TEMP_README" "$README_FILE"
        echo "Updated README.md for Day ${DAY} - Part 1 description updated (basic mode)"
    else
        # Update both parts
        if [ "$HAS_PART_2" = "yes" ]; then
            # Update Part 1
            sed -i.bak "/^## Part 1$/,/^## Part 2$/{ /^### Problem$/,/^### Solution$/{ /^### Solution$/!d; } }" "$TEMP_README"
            sed -i.bak "/^### Problem$/a\\
${PART1}
" "$TEMP_README"
            # Update Part 2
            sed -i.bak "/^## Part 2$/,/^### Solution$/{ /^### Problem$/,/^### Solution$/{ /^### Solution$/!d; } }" "$TEMP_README"
            sed -i.bak "/^### Problem$/a\\
${PART2}
" "$TEMP_README"
            rm -f "${TEMP_README}.bak"
            mv "$TEMP_README" "$README_FILE"
            echo "Updated README.md for Day ${DAY}"
            echo "  - Both Part 1 and Part 2 descriptions included (basic mode)"
        else
            # Only Part 1
            sed -i.bak "/^## Part 1$/,/^## Part 2$/{ /^### Problem$/,/^### Solution$/{ /^### Solution$/!d; } }" "$TEMP_README"
            sed -i.bak "/^### Problem$/a\\
${PART1}
" "$TEMP_README"
            rm -f "${TEMP_README}.bak"
            mv "$TEMP_README" "$README_FILE"
            echo "Updated README.md for Day ${DAY}"
            echo "  - Part 1 description included (Part 2 not yet available) (basic mode)"
        fi
    fi
fi
