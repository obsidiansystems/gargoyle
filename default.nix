{pkgs ? null, postgresql ? null}: self: super:
  let pkgs' = if pkgs == null then self.callPackage ({pkgs}: pkgs) {} else pkgs;
      postgresql' = if postgresql == null then pkgs'.postgresql else postgresql;
  in {
  gargoyle = self.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = self.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql {};
  gargoyle-postgresql-nix = pkgs'.haskell.lib.overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-nix" ./gargoyle-postgresql-nix {})
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (if postgresql' == null then pkgs'.postgresql else postgresql') ];
    });
  gargoyle-postgresql-nix-monitor = pkgs'.haskell.lib.justStaticExecutables (pkgs'.haskell.lib.overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-nix-monitor" ./gargoyle-postgresql-nix-monitor {})
    (drv:{
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (pkgs'.haskell.lib.justStaticExecutables self.gargoyle-postgresql-nix) ];
    }));
  gargoyle-postgresql-connect = pkgs'.haskell.lib.justStaticExecutables (pkgs'.haskell.lib.overrideCabal
    (self.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect {})
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [(pkgs'.haskell.lib.justStaticExecutables self.gargoyle-postgresql-nix)];
    }));
}
