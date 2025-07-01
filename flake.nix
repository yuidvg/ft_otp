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
        let
          # Helper function to create test derivations
          mkTestScript = name: script: deps: pkgs.stdenv.mkDerivation {
            inherit name;
            src = ./.;
            nativeBuildInputs = deps ++ [ pkgs.bash ];
            buildPhase = ''
              # Make script executable and run test
              chmod +x ${script}
              ./${script} $PWD/test/key.hex
            '';
            installPhase = ''
              mkdir -p $out
              echo "${name} completed successfully" > $out/result
            '';
          };
        in
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
          checks.e2e-test = mkTestScript
            "ft-otp-e2e-test"
            "test/e2e-test.sh"
            [ self'.packages.ft-otp pkgs.oathToolkit ];

          # HOTP deterministic test
          checks.hotp-deterministic-test = mkTestScript
            "ft-otp-hotp-test"
            "test/hotp-test.sh"
            [ pkgs.oathToolkit ];

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
