haskellPackages: rec {
  gargoyle = haskellPackages.callPackage ./gargoyle {};
  gargoyle-postgresql = haskellPackages.callPackage ./gargoyle-postgresql { inherit gargoyle; };
  gargoyle-postgresql-nix = haskellPackages.callPackage ./gargoyle-postgresql-nix { inherit gargoyle gargoyle-postgresql; };
  gargoyle-postgresql-connect = haskellPackages.callPackage ./gargoyle-postgresql-connect { inherit gargoyle gargoyle-postgresql gargoyle-postgresql-nix; };
}
