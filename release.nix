{ nixpkgs ? import ./dep/nixpkgs {} }:
let haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: import ./. self;
    };
in {
  inherit (haskellPackages)
    gargoyle
    gargoyle-postgresql
    gargoyle-postgresql-nix
    gargoyle-postgresql-connect;
}
