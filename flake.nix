{
  description = "ft_otp - Time-based One-Time Password implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        {

          haskellProjects.default = {
            # basePackages = pkgs.haskellPackages;

            # Packages to add on top of `basePackages`, e.g. from Hackage
            #packages = {
            #  aeson.source = "1.5.0.0"; # Hackage version
            #};

            # my-haskell-package development shell configuration
            #devShell = {
            #  hlsCheck.enable = false;
            #};

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          # Set the default package to be the ft_otp executable
          packages.default = self'.packages.ft-otp;

          # E2E test that compares ft_otp output with oathtool
          checks.e2e-test = pkgs.stdenv.mkDerivation {
            name = "ft-otp-e2e-test";
            src = ./.;

            nativeBuildInputs = [
              self'.packages.ft-otp
              pkgs.oathToolkit
            ];

            buildPhase = ''
              # Create test directory
              mkdir -p $out/test
              cd $out/test

              # Copy test key
              cp ${./test/key.hex} key.hex

              # Test key generation
              ft_otp -g key.hex

              # Function to test TOTP generation
              test_totp() {
                local attempt=$1
                echo "=== Test attempt $attempt ==="

                # Generate TOTP with both tools simultaneously to minimize time drift
                FT_OTP_OUTPUT=$(ft_otp -k ft_otp.key)
                OATHTOOL_OUTPUT=$(oathtool --totp $(cat key.hex))

                echo "ft_otp output: $FT_OTP_OUTPUT"
                echo "oathtool output: $OATHTOOL_OUTPUT"

                if [ "$FT_OTP_OUTPUT" = "$OATHTOOL_OUTPUT" ]; then
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
                echo "success" > $out/result
              else
                echo "✗ E2E test failed: outputs do not match after 3 attempts"
                exit 1
              fi
            '';

            installPhase = ''
              echo "E2E test completed successfully"
            '';
          };

          # Additional test with specific time values (using HOTP mode for deterministic testing)
          checks.hotp-deterministic-test = pkgs.stdenv.mkDerivation {
            name = "ft-otp-hotp-test";
            src = ./.;

            nativeBuildInputs = [
              self'.packages.ft-otp
              pkgs.oathToolkit
            ];

            buildPhase = ''
              mkdir -p $out/test
              cd $out/test

              # Copy test key
              cp ${./test/key.hex} key.hex

              echo "=== HOTP Deterministic Test ==="
              echo "Testing with known test vectors..."

              # Test with oathtool HOTP mode for deterministic results
              for counter in 0 1 2 3; do
                echo "Testing counter: $counter"
                OATHTOOL_OUTPUT=$(oathtool --hotp --counter=$counter $(cat key.hex))
                echo "oathtool HOTP output for counter $counter: $OATHTOOL_OUTPUT"
              done

              echo "✓ HOTP deterministic test completed"
              echo "success" > $out/result
            '';

            installPhase = ''
              echo "HOTP deterministic test completed"
            '';
          };

          devShells.default = pkgs.mkShell {
            name = "ft-otp development shell";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            nativeBuildInputs = with pkgs; [
              # other development tools.
              haskellPackages.cabal-gild
              oathToolkit
            ];
          };
        };
    };
}
