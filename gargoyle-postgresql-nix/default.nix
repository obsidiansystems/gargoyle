{ mkDerivation, base, bytestring, gargoyle, gargoyle-postgresql
, process, shelly, stdenv, template-haskell, text, postgresql
}:
mkDerivation {
  pname = "gargoyle-postgresql-nix";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring gargoyle gargoyle-postgresql process shelly
    template-haskell text
  ];
  librarySystemDepends = [
    postgresql
  ];
  executableHaskellDepends = [ base gargoyle gargoyle-postgresql ];
  description = "Manage PostgreSQL servers with gargoyle and nix";
  license = stdenv.lib.licenses.bsd3;
}
