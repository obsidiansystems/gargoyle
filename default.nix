{ haskellPackages
, pkgs ? haskellPackages.callPackage ({pkgs}: pkgs) {}
, postgresql ? pkgs.postgresql
, ...
}: {
  gargoyle = haskellPackages.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql {})
    (drv: {
      testSystemDepends = (drv.testSystemDepends or []) ++ [ (if postgresql == null then pkgs.postgresql else postgresql) ];
    });
  gargoyle-postgresql-nix = pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "gargoyle-postgresql-nix" ./gargoyle-postgresql-nix {})
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (if postgresql == null then pkgs.postgresql else postgresql) ];
    });
  gargoyle-postgresql-connect = haskellPackages.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect {};
  gargoyle-nix-postgres-monitor = pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "gargoyle-nix-postgres-monitor" ./gargoyle-nix-postgres-monitor {}) (drv: {
    librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (if postgresql == null then pkgs.postgresql else postgresql) ];
  });
}
