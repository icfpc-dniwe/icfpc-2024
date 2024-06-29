{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage ./. {});

  shell = drv.env.overrideAttrs (self: {
    nativeBuildInputs = self.nativeBuildInputs ++ [ haskellPackages.cabal-install haskellPackages.hpack ];
  });

in

  if pkgs.lib.inNixShell then shell else drv
