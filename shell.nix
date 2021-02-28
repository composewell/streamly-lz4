{ nixpkgs ? import <nixpkgs> {config.allowBroken = true;}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, exceptions, fusion-plugin
      , fusion-plugin-types, gauge, hspec, QuickCheck, stdenv, streamly
      , temporary, cabal-doctest
      }:
      mkDerivation {
        pname = "streamly-lz4";
        version = "0.1.0";
        src = ./.;
        configureFlags = [ "-ffusion-plugin" ];
        libraryHaskellDepends = [
          base exceptions fusion-plugin-types streamly cabal-doctest
        ];
        testHaskellDepends = [ base hspec QuickCheck streamly temporary ];
        benchmarkHaskellDepends = [
          base directory fusion-plugin gauge streamly
        ];
        doBenchmark = true;
        homepage = "https://github.com/composewell/streamly-lz4";
        description = "Streamly combinators for LZ4 compression";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
