{
  description = "ft_otp - Time-based One-Time Password implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc96;

        ft-otp = haskellPackages.callCabal2nix "ft-otp" ./. {};

      in {
        packages = {
          default = ft-otp;
          ft_otp = ft-otp;
        };

        apps = {
          default = {
            type = "app";
            program = "${ft-otp}/bin/ft_otp";
          };
          ft_otp = {
            type = "app";
            program = "${ft-otp}/bin/ft_otp";
          };
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ ft-otp ];
          buildInputs = with haskellPackages; [
            cabal-install
            hlint
            haskell-language-server
          ];
        };
      });
}
