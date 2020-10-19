self: super:
   let pkgs = self.callPackage ({pkgs}:pkgs) {};
   in with pkgs.haskell.lib; {postgresql ? pkgs.postgresql}: {
  gargoyle = self.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = self.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql {};
  gargoyle-postgresql-nix = overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-nix" ./gargoyle-postgresql-nix {})
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ postgresql ];
    });
  gargoyle-postgresql-nix-monitor = justStaticExecutables (overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-nix-monitor" ./gargoyle-postgresql-nix-monitor {})
    (drv:{
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ self.gargoyle-postgresql-nix ];
    }));
  gargoyle-postgresql-connect =
    justStaticExecutables (self.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect {});
}
