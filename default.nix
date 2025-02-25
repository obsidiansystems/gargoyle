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

  gargoyle-postgresql-nix = haskellPackages.callCabal2nix "gargoyle-postgresql-nix" ./gargoyle-postgresql-nix {
    # TODO: libpq will become standalone in https://github.com/NixOS/nixpkgs/pull/359659
    libpq = postgresql;
  };

  gargoyle-postgresql-connect = haskellPackages.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect {};
}
