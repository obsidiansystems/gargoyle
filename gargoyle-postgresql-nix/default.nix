{ mkDerivation, base, bytestring, gargoyle, gargoyle-postgresql
, process, shelly, stdenv, template-haskell, text, postgresql
}:
mkDerivation {
  pname = "gargoyle-nix";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring gargoyle gargoyle-postgresql process shelly
    template-haskell text
  ];
  executableHaskellDepends = [ base gargoyle postgresql ];
  description = "Manage PostgreSQL servers with gargoyle and nix";
  license = stdenv.lib.licenses.unfree;
}
