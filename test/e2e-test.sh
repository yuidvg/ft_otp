#!/usr/bin/env bash

set -euo pipefail

# E2E test script for ft_otp
# Compares ft_otp output with oathtool reference implementation

echo "=== ft_otp E2E Test ==="

# Check if required tools are available
if ! command -v ft_otp &> /dev/null; then
    echo "Error: ft_otp not found in PATH"
    exit 1
fi

if ! command -v oathtool &> /dev/null; then
    echo "Error: oathtool not found in PATH"
    exit 1
fi

# Get the script directory to resolve relative paths correctly
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Test setup
TEST_DIR=$(mktemp -d)
trap 'rm -rf "$TEST_DIR"' EXIT

cd "$TEST_DIR"

# Use provided key file or default to test/key.hex
KEY_FILE="${1:-$PROJECT_ROOT/test/key.hex}"

if [ ! -f "$KEY_FILE" ]; then
    echo "Error: Key file $KEY_FILE not found"
    exit 1
fi

cp "$KEY_FILE" key.hex

echo "Test directory: $TEST_DIR"
echo "Using key file: $KEY_FILE"

# Test key generation
echo "Testing key generation..."
ft_otp -g key.hex

# Function to test TOTP generation
test_totp() {
    local attempt=$1
    echo "=== Test attempt $attempt ==="

    # Generate TOTP with both tools simultaneously to minimize time drift
    local ft_otp_output
    local oathtool_output

    ft_otp_output=$(ft_otp -k ft_otp.key)
    oathtool_output=$(oathtool --totp "$(cat key.hex)")

    echo "ft_otp output: $ft_otp_output"
    echo "oathtool output: $oathtool_output"

    if [ "$ft_otp_output" = "$oathtool_output" ]; then
        echo "✓ Test attempt $attempt passed: outputs match"
        return 0
    else
        echo "✗ Test attempt $attempt failed: outputs do not match"
        return 1
    fi
}

# Try multiple times to account for time window boundaries
SUCCESS=0
for i in 1 2 3; do
    if test_totp $i; then
        SUCCESS=1
        break
    fi

    # Wait a bit and try again (in case we hit a time boundary)
    if [ $i -lt 3 ]; then
        echo "Waiting 2 seconds before retry..."
        sleep 2
    fi
done

if [ $SUCCESS -eq 1 ]; then
    echo "✓ E2E test passed: ft_otp and oathtool outputs match"
    exit 0
else
    echo "✗ E2E test failed: outputs do not match after 3 attempts"
    exit 1
fi