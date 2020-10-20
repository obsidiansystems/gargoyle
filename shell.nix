{ pkgs ? import dep/nixpkgs {}
, haskellPackages ? pkgs.haskellPackages
, ...
}:
haskellPackages.extend (import ./. {})
