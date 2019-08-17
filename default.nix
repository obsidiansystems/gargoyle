haskellPackages: rec {
  gargoyle = haskellPackages.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = haskellPackages.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql { };
  gargoyle-postgresql-nix = haskellPackages.callPackage ./gargoyle-postgresql-nix { };
  gargoyle-postgresql-connect = haskellPackages.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect { };
}
