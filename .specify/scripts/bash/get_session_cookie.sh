#!/bin/bash

# get_session_cookie.sh
# Manages AOC session cookie with smart fallback logic:
# 1. Check if cookie is stored in .env file
# 2. Check if cookie is in environment variable
# 3. Get cookie from browser (if available)
# 4. Check if cookie has expired and refresh if needed

# Note: set -e is not used here because some functions are expected to fail

YEAR=2025
ENV_FILE="$(git rev-parse --show-toplevel)/.env"
STATE_KEY="AOC_SESSION"

# Function to get cookie from browser (macOS - Safari)
get_cookie_from_chrome() {
    local domain="adventofcode.com"
    
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # Try Safari first using browser_cookie3
        if command -v python3 &> /dev/null; then
            local cookie=$(python3 -c "
import sys
try:
    import browser_cookie3
    # Try Safari first
    try:
        cj = browser_cookie3.safari(domain_name='${domain}')
        for cookie in cj:
            if cookie.name == 'session':
                print(cookie.value)
                sys.exit(0)
    except Exception as e:
        pass
    
    # Fallback to Chrome if Safari doesn't work
    try:
        cj = browser_cookie3.chrome(domain_name='${domain}')
        for cookie in cj:
            if cookie.name == 'session':
                print(cookie.value)
                sys.exit(0)
    except Exception as e:
        pass
    sys.exit(1)
except ImportError:
    sys.exit(1)
" 2>/dev/null)
            
            if [ -n "$cookie" ] && [ "$cookie" != "" ]; then
                echo "$cookie"
                return 0
            fi
        fi
    fi
    
    return 1
}

# Function to validate cookie by making a test request
validate_cookie() {
    local cookie=$1
    local test_url="https://adventofcode.com/${YEAR}/day/1/input"
    
    local response=$(curl -s -w "\n%{http_code}" -H "Cookie: session=${cookie}" "${test_url}" 2>/dev/null)
    local http_code=$(echo "$response" | tail -n1)
    
    # 200 = valid, 404 = day doesn't exist but cookie is valid, 400/401 = invalid cookie
    if [[ "$http_code" == "200" ]] || [[ "$http_code" == "404" ]]; then
        return 0
    else
        return 1
    fi
}

# Function to prompt user for cookie
prompt_for_cookie() {
    echo "Please provide your AOC session cookie:"
    echo "1. Go to https://adventofcode.com"
    echo "2. Open browser DevTools (F12)"
    echo "3. Go to Application/Storage > Cookies > adventofcode.com"
    echo "4. Copy the 'session' cookie value"
    echo ""
    read -p "Session cookie: " cookie
    echo "$cookie"
}

# Main logic: Check state, env, then browser, then prompt
get_cookie() {
    local cookie=""
    
    # 1. Check .env file
    if [ -f "$ENV_FILE" ]; then
        cookie=$(grep "^${STATE_KEY}=" "$ENV_FILE" | cut -d'=' -f2- | sed 's/^"//;s/"$//' | sed "s/^'//;s/'$//")
        if [ -n "$cookie" ] && validate_cookie "$cookie"; then
            echo "$cookie"
            return 0
        elif [ -n "$cookie" ]; then
            echo "Cookie in .env file appears to be expired." >&2
        fi
    fi
    
    # 2. Check environment variable
    if [ -z "$cookie" ] && [ -n "${AOC_SESSION:-}" ]; then
        cookie="$AOC_SESSION"
        if validate_cookie "$cookie"; then
            # Save to .env for future use
            save_cookie_to_env "$cookie"
            echo "$cookie"
            return 0
        else
            echo "Cookie in environment variable appears to be expired." >&2
        fi
    fi
    
    # 3. Try to get from browser (may not work on all platforms)
    cookie=$(get_cookie_from_chrome 2>/dev/null || echo "")
    if [ -n "$cookie" ] && validate_cookie "$cookie"; then
        save_cookie_to_env "$cookie"
        echo "$cookie"
        return 0
    fi
    
    # 4. Prompt user
    cookie=$(prompt_for_cookie)
    if [ -n "$cookie" ] && validate_cookie "$cookie"; then
        save_cookie_to_env "$cookie"
        echo "$cookie"
        return 0
    else
        echo "Error: Invalid cookie provided." >&2
        exit 1
    fi
}

# Function to save cookie to .env file
save_cookie_to_env() {
    local cookie=$1
    local dir=$(dirname "$ENV_FILE")
    
    # Create .env file if it doesn't exist
    mkdir -p "$dir"
    
    # Add or update the cookie in .env
    if [ -f "$ENV_FILE" ] && grep -q "^${STATE_KEY}=" "$ENV_FILE"; then
        # Update existing entry
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS sed
            sed -i '' "s|^${STATE_KEY}=.*|${STATE_KEY}=${cookie}|" "$ENV_FILE"
        else
            # Linux sed
            sed -i "s|^${STATE_KEY}=.*|${STATE_KEY}=${cookie}|" "$ENV_FILE"
        fi
    else
        # Append new entry
        echo "${STATE_KEY}=${cookie}" >> "$ENV_FILE"
    fi
    
    # Ensure .env is in .gitignore (it should be, but double-check)
    if ! grep -q "^\.env$" .gitignore 2>/dev/null; then
        echo ".env" >> .gitignore
    fi
}

# Main execution
# If script is sourced, export AOC_SESSION
# If script is executed, output cookie and export it
if [ "${BASH_SOURCE[0]}" != "${0}" ]; then
    # Script is being sourced
    if [ -z "${AOC_SESSION:-}" ]; then
        # Redirect stderr to /dev/null, capture only stdout (the cookie)
        COOKIE=$(get_cookie 2>/dev/null)
        export AOC_SESSION="$COOKIE"
    fi
else
    # Script is being executed directly
    # Redirect stderr to /dev/null, capture only stdout (the cookie)
    COOKIE=$(get_cookie 2>/dev/null)
    export AOC_SESSION="$COOKIE"
    echo "Session cookie retrieved and saved to .env" >&2
    echo "$COOKIE"
fi
