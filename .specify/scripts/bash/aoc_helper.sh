#!/bin/bash

# Advent of Code helper script
# This script helps fetch AOC problem descriptions and input files

# Configuration
YEAR=2025
SESSION_COOKIE="${AOC_SESSION:-}"

if [ -z "$SESSION_COOKIE" ]; then
    echo "Error: AOC_SESSION environment variable not set."
    echo "Please set it to your Advent of Code session cookie."
    echo "You can find it in your browser's cookies for adventofcode.com"
    exit 1
fi

fetch_day() {
    local day=$1
    local url="https://adventofcode.com/${YEAR}/day/${day}"
    
    echo "Fetching problem description for Day ${day}..."
    
    # Fetch problem description (requires session cookie)
    curl -s -H "Cookie: session=${SESSION_COOKIE}" "${url}" > /tmp/aoc_day_${day}.html
    
    if [ $? -eq 0 ]; then
        echo "Successfully fetched Day ${day} problem description"
        echo "Saved to /tmp/aoc_day_${day}.html"
    else
        echo "Error fetching Day ${day}"
        return 1
    fi
}

fetch_input() {
    local day=$1
    local output_file="${2:-days/day_$(printf %02d $day)/data/input.txt}"
    local url="https://adventofcode.com/${YEAR}/day/${day}/input"
    
    echo "Fetching input for Day ${day}..."
    
    # Create directory if it doesn't exist
    mkdir -p "$(dirname "$output_file")"
    
    # Fetch input (requires session cookie)
    curl -s -H "Cookie: session=${SESSION_COOKIE}" "${url}" > "${output_file}"
    
    if [ $? -eq 0 ]; then
        echo "Successfully fetched input for Day ${day}"
        echo "Saved to ${output_file}"
    else
        echo "Error fetching input for Day ${day}"
        return 1
    fi
}

case "$1" in
    fetch-day)
        fetch_day "$2"
        ;;
    fetch-input)
        fetch_input "$2" "$3"
        ;;
    *)
        echo "Usage: $0 {fetch-day|fetch-input} <day_number> [output_file]"
        echo ""
        echo "Examples:"
        echo "  $0 fetch-day 1"
        echo "  $0 fetch-input 1"
        exit 1
        ;;
esac
