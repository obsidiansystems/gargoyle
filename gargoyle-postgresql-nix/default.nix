haskellPackages: nixpkgs:
  nixpkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "gargoyle-postgresql-nix" ./. { }) (drv: {
    librarySystemDepends = (drv.librarySystemDepends or []) ++ [ nixpkgs.postgresql ];
  })
