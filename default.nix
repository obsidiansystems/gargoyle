haskellPackages: rec {
  gargoyle = haskellPackages.callPackage ./gargoyle {};
  gargoyle-postgresql = haskellPackages.callPackage ./gargoyle-postgresql { inherit gargoyle; };
  gargoyle-postgresql-nix = haskellPackages.callPackage ./gargoyle-postgresql-nix { inherit gargoyle gargoyle-postgresql; };
}
