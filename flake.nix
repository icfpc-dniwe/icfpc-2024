{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    poetry2nix.url = "github:nix-community/poetry2nix";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    poetry2nix,
  }:
    {
      overlays.default = final: prev: {
        icfpc-2024 = final.python3.pkgs.callPackage ./. {};
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [poetry2nix.overlays.default self.overlays.default];
      };
    in {
      packages.default = pkgs.icfpc-2024;
    });
}
