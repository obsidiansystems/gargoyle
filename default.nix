haskellPackages: nixpkgs: rec {
  which = haskellPackages.callHackageDirect {
    pkg = "which";
    ver = "0.1.0.0";
    sha256 = "1c8svdiv378ps63lwn3aw7rv5wamlpmzgcn21r2pap4sx7p08892";
  } {};

  gargoyle = haskellPackages.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = haskellPackages.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql { };
  gargoyle-postgresql-nix = import ./gargoyle-postgresql-nix haskellPackages nixpkgs;
  gargoyle-postgresql-connect = haskellPackages.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect { };
}
