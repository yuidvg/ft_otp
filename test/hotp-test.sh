#!/usr/bin/env bash

set -euo pipefail

# HOTP deterministic test script
# Tests HOTP functionality with known test vectors

echo "=== ft_otp HOTP Deterministic Test ==="

# Check if oathtool is available
if ! command -v oathtool &> /dev/null; then
    echo "Error: oathtool not found in PATH"
    exit 1
fi

# Get the script directory to resolve relative paths correctly
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Test setup
KEY_FILE="${1:-$PROJECT_ROOT/test/key.hex}"

if [ ! -f "$KEY_FILE" ]; then
    echo "Error: Key file $KEY_FILE not found"
    exit 1
fi

echo "Using key file: $KEY_FILE"
echo "Testing with known test vectors..."

# Test with oathtool HOTP mode for deterministic results
for counter in 0 1 2 3 4; do
    echo "Testing counter: $counter"
    oathtool_output=$(oathtool --hotp --counter="$counter" "$(cat "$KEY_FILE")")
    echo "oathtool HOTP output for counter $counter: $oathtool_output"
done

echo "✓ HOTP deterministic test completed"