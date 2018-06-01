haskellPackages: rec {
  gargoyle = haskellPackages.callPackage ./gargoyle {};
  gargoyle-postgresql = haskellPackages.callPackage ./gargoyle-postgresql {};
  gargoyle-nix = haskellPackages.callPackage ./gargoyle-nix { inherit gargoyle gargoyle-postgresql; };
}
