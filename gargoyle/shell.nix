{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, filepath, network, process
      , stdenv, unix
      }:
      mkDerivation {
        pname = "gargoyle";
        version = "0.1";
        src = ./.;
        libraryHaskellDepends = [
          base directory filepath network process unix
        ];
        description = "Automatically spin up and spin down local daemons";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
