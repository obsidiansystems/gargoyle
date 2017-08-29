haskellPackages: {
  gargoyle = haskellPackages.callPackage ./gargoyle {};
  gargoyle-postgresql = haskellPackages.callPackage ./gargoyle-postgresql {};
}
