{ mkDerivation, array, attoparsec, base, doctest, lens, linear, mtl
, recursion-schemes, stdenv, text, vector
}:
mkDerivation {
  pname = "adventOfCode2019";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base doctest lens linear mtl recursion-schemes
    text vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  description = "Advent of code 2019";
  license = stdenv.lib.licenses.bsd3;
}
