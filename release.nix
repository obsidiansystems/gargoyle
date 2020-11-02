{ nixpkgs ? import ./dep/nixpkgs {} }:
let
  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: import ./. { haskellPackages = self; } // {
      which = haskellPackages.callHackageDirect {
        pkg = "which";
        ver = "0.2";
        sha256 = "sha256:1g795yq36n7c6ycs7c0799c3cw78ad0cya6lj4x08m0xnfx98znn";
      } {};
    };
  };
in {
  inherit (haskellPackages)
    gargoyle
    gargoyle-postgresql
    gargoyle-postgresql-nix
    gargoyle-postgresql-connect;
}
