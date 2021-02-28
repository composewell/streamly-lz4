# nix-shell --argstr c2nix "--benchmark --flag fusion-plugin"
{ nixpkgs ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "default"
, c2nix ? "--benchmark" }:
let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  mkPkgGit =
    super: gitSrc: ref: name:
    let src =
          builtins.fetchGit {
            url = gitSrc;
            ref = ref;
          };
    in super.callCabal2nix name src {};

  overiddenPackages =
    haskellPackages.override {
      overrides =
        _: super: {
          streamly =
            let src = "https://github.com/composewell/streamly.git";
                ref = "c8b9fac92bb0d08cb87f601882c9c634ed09af35";
            in mkPkgGit super src ref "streamly";
        };
    };

  drv = inShell:
    let orig =
          overiddenPackages.callCabal2nixWithOptions "streamly-lz4" ./. c2nix {};
    in if inShell
       then orig.overrideAttrs (oldAttrs: { src = null; })
       else orig;

in
if pkgs.lib.inNixShell then (drv true).env else drv false
