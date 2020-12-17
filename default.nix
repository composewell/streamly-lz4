{ nixpkgs ? import <nixpkgs> {}
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
                ref = "9fefed37d5da5aa103f93ded65b1335903651e26";
            in mkPkgGit super src ref "streamly";
          fusion-plugin =
            let src = "https://github.com/composewell/fusion-plugin";
                ref = "1ad15ebf56a91ec967c48f7e5620f74131a947ab";
            in mkPkgGit super src ref "fusion-plugin";
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
