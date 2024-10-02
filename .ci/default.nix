{ }:
let
  nixos2311 = import ./nixpkgs/23.11 {};
  pkgs = nixos2311;
  inherit (pkgs) lib;
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken overrideCabal;
  ghcs = {

    "ghc8107" = pkgs;
    "ghc902" = pkgs;
    "ghc928" = pkgs;
    "ghc948" = pkgs;
    "ghc963" = pkgs;
    "ghc981" = import ./nixpkgs/unstable {};
  };
  build =
    { nixpkgs
    , ghc ? null
    }:
    let
      baseHaskellPackages = if ghc != null then nixpkgs.haskell.packages.${ghc} else nixpkgs.haskellPackages;
      haskellPackages = baseHaskellPackages.override {
        overrides = self: super: import ../. { haskellPackages = self; };
      };
    in {
      inherit (haskellPackages)
        gargoyle
        gargoyle-postgresql
        gargoyle-postgresql-nix
        gargoyle-postgresql-connect
        gargoyle-nix-postgres-monitor;
    };
in
  lib.mapAttrs (ghc: nixpkgs: lib.recurseIntoAttrs (build { inherit nixpkgs ghc; })) ghcs
