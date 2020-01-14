{ nixpkgs ? import ./dep/nixpkgs {} }:
let
  haskellPackages = nixpkgs.haskellPackages.extend (self: super: import ./. self // {
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.1.0.0";
      sha256 = "1c8svdiv378ps63lwn3aw7rv5wamlpmzgcn21r2pap4sx7p08892";
    } {};
  });
in {
  inherit (haskellPackages)
    gargoyle
    gargoyle-postgresql
    gargoyle-postgresql-nix
    gargoyle-postgresql-connect;
}
