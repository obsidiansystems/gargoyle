{ pkgs ? import <nixpkgs> {}
, haskellPackages ? pkgs.haskellPackages
, ...
}:
haskellPackages.extend (self: super: import ./. self)
