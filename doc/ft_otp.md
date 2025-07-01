# Cybersecurity Piscine

## ft_otp

```
Summary: Nothing ever lasts forever...
```

## Contents

- Introduction
- Mandatory Part
- Testing

## Introduction

Passwords are one of the biggest headaches in computer security Users forget
them, share them, reuse them and choose them horribly bad. Furthermore, passwords
are sooner or later leaked in security breaches. One way to avoid this is to use one-time
passwords, based on timestamps, which expire after a few minutes and then become in-
valid. Whether you already use this system, or if you have never heard of it, it is quite
likely that one of your passwords has been compromised at some point in your life.

In this project, the aim isto implement a TOTP(Time-based One-Time Password)
system, which will be capable of generating ephemeral passwords from a master key.
It will be based on theRFC:https://datatracker.ietf.org/doc/html/rfc6238, so
you could use it in your day to day.

## Mandatory Part

In the language of your choice, you have to implement a program that allows you to store
an initial password in file, and that is capable of generating a new one time password
every time it is requested.

You can use any library that facilitates the implementation of the algorithm, as long
as it doesn't do the dirty work, i.e. using a TOTP library is strictly prohibited. Of
course, you can and should use a library or function that allows you to access system
time.

- The executable must be namedft_otp
- Your program must take arguments.

```
◦ -g: The program receives as argument a hexadecimal key of at least 64 char-
acters. The program stores this key safely in a file calledft_otp.key, which
is encrypted.
◦ -k: The program generates a new temporary password based on the key given
as argument and prints it on the standard output.
```
- Your program must use the HOTP algorithm (RFC 4226).
- The generated one-time password must be random and must always contain the
    same format, i.e. 6 digits.

```
Below is an example of use:
```
```
$ echo -n "NEVER GONNA GIVE YOU UP" > key.txt
$ ./ft_otp -g key.txt
./ft_otp: error: key must be 64 hexadecimal characters.
$ [..]
$ cat key.hex | wc -c
64
$ ./ft_otp -g key.hex
Key was successfully saved in ft_otp.key.
$ ./ft_otp -k ft_otp.key
836492
$ sleep 60
$ ./ft_otp -k ft_otp.key
123518
```

Cybersecurity Piscine ft_otp

You can check if your program is working properly by comparing generated passwords
withOathtoolor any tool of your choice.

```
oathtool –totp $(cat key.hex)
```
```
Please note that the reference software you choose will be used
during your evaluation.
```

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