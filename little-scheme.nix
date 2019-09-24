{ mkDerivation, base, haskeline, hspec, mtl, parsec, stdenv }:
mkDerivation {
  pname = "little-scheme";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl parsec ];
  executableHaskellDepends = [ base haskeline mtl parsec ];
  testHaskellDepends = [ base hspec mtl ];
  doHaddock = false;
  license = stdenv.lib.licenses.mit;
}
