
## Testing

This project implements comprehensive End-to-End (E2E) testing using Nix and haskell-flake to ensure compatibility with the reference implementation (oathtool).

### Test Environment

The testing environment is managed through Nix flakes, providing:
- **Ultimate Reproducibility**: Complete environment definition (OS, libraries, specific versions of oathtool and ft_otp, test files)
- **Automatic Dependency Management**: Both system tools (oathtool) and custom programs (ft_otp) are managed by Nix
- **CI/CD Integration**: Tests can be run consistently across different environments

### Test Structure

#### 1. E2E Test (`test/e2e-test.sh`)
Compares ft_otp output with oathtool reference implementation:
- Tests key generation functionality
- Generates TOTP with both ft_otp and oathtool simultaneously
- Compares outputs with retry logic to handle time window boundaries
- Uses temporary directories for isolation

#### 2. HOTP Deterministic Test (`test/hotp-test.sh`)
Tests HOTP functionality with known test vectors:
- Uses oathtool in HOTP mode for deterministic results
- Tests multiple counter values (0-4)
- Provides baseline for HOTP algorithm verification

### Running Tests

#### Using Nix (Recommended)
```bash
# Run all tests
nix flake check

# Run individual tests
nix build .#checks.aarch64-darwin.e2e-test
nix build .#checks.aarch64-darwin.hotp-deterministic-test
```

#### Manual Testing in Development Environment
```bash
# Enter development shell
nix develop

# Build the project
cabal build

# Run tests manually
./test/e2e-test.sh
./test/hotp-test.sh
```

### Test Implementation Details

The tests are implemented following functional programming and Test-Driven Development (TDD) principles:

1. **Separation of Concerns**: Test logic is separated into dedicated shell scripts, while Nix configuration handles environment setup
2. **Reusability**: Helper functions (`mkTestScript`) reduce duplication in test definitions
3. **Error Handling**: Comprehensive error checking for missing dependencies and files
4. **Path Resolution**: Robust path handling for different execution contexts

### TDD Workflow

The testing framework supports the Red-Green-Refactor cycle:
- **Red**: Tests fail when implementation doesn't match oathtool behavior
- **Green**: Implementation is corrected to pass tests
- **Refactor**: Code is improved while maintaining test compatibility

### Test Configuration

Tests are defined in `flake.nix` using the `mkTestScript` helper function:

```nix
checks.e2e-test = mkTestScript
  "ft-otp-e2e-test"
  "test/e2e-test.sh"
  [ self'.packages.ft-otp pkgs.oathToolkit ];
```

This approach ensures that:
- Tests are reproducible across environments
- Dependencies are automatically managed
- Test results are consistent and reliable
