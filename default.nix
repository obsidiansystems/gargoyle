{ haskellPackages
, pkgs ? haskellPackages.callPackage ({pkgs}: pkgs) {}
, postgresql ? pkgs.postgresql
}: rec {
  gargoyle = haskellPackages.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = haskellPackages.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql {};
  gargoyle-postgresql-nix = pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "gargoyle-postgresql-nix" ./gargoyle-postgresql-nix {})
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (if postgresql == null then pkgs.postgresql else postgresql) ];
    });
  gargoyle-postgresql-nix-monitor = pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "gargoyle-postgresql-nix-monitor" ./gargoyle-postgresql-nix-monitor {})
    (drv:{
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (pkgs.haskell.lib.justStaticExecutables gargoyle-postgresql-nix) ];
    });
  gargoyle-postgresql-connect = haskellPackages.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect {};
}
