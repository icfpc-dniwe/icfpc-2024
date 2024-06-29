{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    poetry2nix.url = "github:nix-community/poetry2nix/master";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    poetry2nix,
    ...
  }: let
    env = {
      lib,
      buildFHSEnv,
      writers,
    }:
    # Move that to NixOverlay later.
      buildFHSEnv {
        name = "icfpc2024_env";

        targetPkgs = pkgs:
          with pkgs; [
            python3
            poetry
            # Native dependencies for the Python libraries.
            zlib
          ];

        profile = ''
          poetry install --with dev
	  source $(poetry env info --path)/bin/activate
        '';

        runScript = writers.writeBash "run-script" ''
          if [ "$#" = 0 ]; then
            exec "$SHELL"
          else
            exec "$@"
          fi
        '';
      };
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [poetry2nix.overlays.default];
      };
    in {
      devShells.default = (pkgs.callPackage env {}).env;
    });
}
