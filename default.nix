{postgresql ? null}: self: super:
   let pkgs = self.callPackage ({pkgs}:pkgs) {};
   in with pkgs.haskell.lib; {
  gargoyle = self.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = self.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql {};
  gargoyle-postgresql-nix = overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-nix" ./gargoyle-postgresql-nix {})
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (if postgresql == null then pkgs.postgresql else postgresql) self.gargoyle-postgresql-nix-monitor ];
    });
  gargoyle-postgresql-nix-monitor = overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-nix-monitor" ./gargoyle-postgresql-nix-monitor {})
    (drv:{
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (if postgresql == null then pkgs.postgresql else postgresql)];
    });
  gargoyle-postgresql-connect =
    self.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect {};
}
