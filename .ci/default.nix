{ }:
let
  nixpkgsSets = import ./nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (unstable) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck markUnbroken overrideCabal;
  ghcs = rec {
    ghc865 = nixos2003;
    ghc884 = nixos2003;
    ghc8107 = unstable;
    ghc902 = unstable;
    ghc928 = unstable;
    ghc946 = unstable;
    ghc962 = unstable;
  };
  build =
    { nixpkgs ? import ./dep/nixpkgs {}
    , ghc ? null
    }:
    let
      baseHaskellPackages = if ghc != null then nixpkgs.haskell.packages.${ghc} else nixpkgs.haskellPackages;
      haskellPackages = baseHaskellPackages.override {
        overrides = self: super: import ../. { haskellPackages = self; } // {
          which = self.callHackageDirect {
            pkg = "which";
            ver = "0.2.0.1";
            sha256 = "0kfbzaz1wgzmyqvw4m57yj43x4ihalk5a7y703fa0pjv0cvvx0ss";
          } {};
          shelly = self.callHackageDirect {
            pkg = "shelly";
            ver = "1.12.1";
            sha256 = "sha256-3Sy7Twinm5+3/nwrfk5uMmWf9z088MZ7533VtyEEB8k=";
          } {};
        };
      };
    in {
      inherit (haskellPackages)
        gargoyle
        gargoyle-postgresql
        gargoyle-postgresql-nix
        gargoyle-postgresql-connect;
    };
in
  lib.mapAttrs (ghc: nixpkgs: lib.recurseIntoAttrs (build { inherit nixpkgs ghc; })) ghcs
