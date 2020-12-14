{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, c2nix ? "--benchmark" }:
let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  drv = inShell:
    let orig =
          haskellPackages.callCabal2nixWithOptions "streamly-lz4" ./. c2nix {};
    in if inShell
       then orig.overrideAttrs (oldAttrs: { src = null; })
       else orig;

in
if pkgs.lib.inNixShell then (drv true).env else drv false
