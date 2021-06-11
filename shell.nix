# nix-shell --argstr c2nix "--benchmark --flag fusion-plugin"
{ nixpkgs ? import <nixpkgs> { }, compiler ? "default", c2nix ? "--benchmark" }:
let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  drv = let
    orig =
      haskellPackages.callCabal2nixWithOptions "streamly-lz4" ./. c2nix { };
  in orig.overrideAttrs (oldAttrs: { src = null; });

in drv.env
