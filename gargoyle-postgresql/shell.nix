{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, directory, gargoyle
      , process, stdenv, stringsearch, text, unix
      }:
      mkDerivation {
        pname = "gargoyle-postgresql";
        version = "0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring directory gargoyle process stringsearch text unix
        ];
        executableHaskellDepends = [
          base bytestring gargoyle process text unix
        ];
        description = "Manage PostgreSQL servers with gargoyle";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
