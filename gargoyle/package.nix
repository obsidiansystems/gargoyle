{ mkDerivation, base, directory, filepath, network, process, stdenv
, unix
}:
mkDerivation {
  pname = "gargoyle";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base directory filepath network process unix
  ];
  description = "Automatically spin up and spin down local daemons";
  license = stdenv.lib.licenses.bsd3;
}
