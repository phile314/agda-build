{ mkDerivation, Agda, base, containers, exceptions, filepath, mtl
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "agda-build";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    Agda base containers exceptions filepath mtl optparse-applicative
    text
  ];
  testHaskellDepends = [ Agda base ];
  homepage = "https://github.com/phile314/agda-build";
  license = stdenv.lib.licenses.bsd3;
}
